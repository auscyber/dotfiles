#include <Carbon/Carbon.h>
#include <libproc.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include <sys/stat.h>

#define kAXExtrasMenuBarAttribute CFSTR("AXExtrasMenuBar")

static bool source_pid_needs_workaround(void)
{
  SInt32 major_version = 0;
  if (Gestalt(gestaltSystemVersionMajor, &major_version) != noErr)
  {
    return false;
  }

  return major_version >= 26;
}

static double point_distance_squared(CGPoint a, CGPoint b)
{
  double dx = a.x - b.x;
  double dy = a.y - b.y;
  return dx * dx + dy * dy;
}

static CGPoint rect_center(CGRect rect)
{
  return CGPointMake(rect.origin.x + rect.size.width / 2.0,
                     rect.origin.y + rect.size.height / 2.0);
}

static bool copy_ax_frame(AXUIElementRef element, CGRect *out_frame)
{
  CFTypeRef position_ref = NULL;
  CFTypeRef size_ref = NULL;
  CGPoint position = CGPointZero;
  CGSize size = CGSizeZero;

  AXError error = AXUIElementCopyAttributeValue(element,
                                                kAXPositionAttribute,
                                                &position_ref);
  if (error != kAXErrorSuccess || !position_ref)
  {
    return false;
  }

  error = AXUIElementCopyAttributeValue(element,
                                        kAXSizeAttribute,
                                        &size_ref);
  if (error != kAXErrorSuccess || !size_ref)
  {
    if (position_ref)
      CFRelease(position_ref);
    return false;
  }

  bool success = false;
  if (AXValueGetValue(position_ref, kAXValueCGPointType, &position) &&
      AXValueGetValue(size_ref, kAXValueCGSizeType, &size))
  {
    out_frame->origin = position;
    out_frame->size = size;
    success = true;
  }

  CFRelease(position_ref);
  CFRelease(size_ref);
  return success;
}

static char *copy_process_name(pid_t pid)
{
  char buffer[PROC_PIDPATHINFO_MAXSIZE];

  if (proc_name(pid, buffer, sizeof(buffer)) > 0)
  {
    return strdup(buffer);
  }

  if (proc_pidpath(pid, buffer, sizeof(buffer)) > 0)
  {
    char *basename = strrchr(buffer, '/');
    if (basename)
      basename++;
    else
      basename = buffer;
    return strdup(basename);
  }

  return NULL;
}

static bool parse_alias(char *alias,
                        char *owner_buffer,
                        size_t owner_buffer_len,
                        char *name_buffer,
                        size_t name_buffer_len,
                        pid_t *pid_out,
                        bool *has_pid_out)
{
  char *first_comma = strchr(alias, ',');
  if (!first_comma)
    return false;

  char *last_comma = strrchr(alias, ',');
  bool has_pid = false;
  pid_t pid = 0;

  if (last_comma && last_comma != first_comma)
  {
    char *pid_end = NULL;
    long parsed_pid = strtol(last_comma + 1, &pid_end, 10);
    if (pid_end && *pid_end == '\0' && parsed_pid > 0)
    {
      has_pid = true;
      pid = (pid_t)parsed_pid;
    }
    else
    {
      last_comma = first_comma;
    }
  }
  else
  {
    last_comma = first_comma;
  }

  size_t owner_len = (size_t)(first_comma - alias);
  if (owner_len >= owner_buffer_len)
    owner_len = owner_buffer_len - 1;
  memcpy(owner_buffer, alias, owner_len);
  owner_buffer[owner_len] = '\0';

  size_t name_len = strlen(first_comma + 1);
  if (has_pid)
  {
    name_len = (size_t)(last_comma - first_comma - 1);
  }
  if (name_len >= name_buffer_len)
    name_len = name_buffer_len - 1;
  memcpy(name_buffer, first_comma + 1, name_len);
  name_buffer[name_len] = '\0';

  if (has_pid && pid_out)
    *pid_out = pid;
  if (has_pid_out)
    *has_pid_out = has_pid;
  return true;
}

struct alias_pid_cache_entry
{
  char owner[256];
  char name[256];
  pid_t pid;
};

static struct alias_pid_cache_entry *g_alias_pid_cache = NULL;
static int g_alias_pid_cache_count = 0;
static uint64_t g_alias_pid_cache_last_refresh_ns = 0;
static const uint64_t ALIAS_PID_CACHE_TTL_NS = 2ULL * 1000ULL * 1000ULL * 1000ULL;
static const char *ALIAS_PID_CACHE_PATH = "/tmp/sketchybar_menus_alias_pid_cache_v1";

static uint64_t monotonic_time_ns(void)
{
  return (uint64_t)clock_gettime_nsec_np(CLOCK_MONOTONIC);
}

static bool alias_pid_cache_upsert(const char *owner,
                                   const char *name,
                                   pid_t pid)
{
  for (int i = 0; i < g_alias_pid_cache_count; ++i)
  {
    if (strcmp(g_alias_pid_cache[i].owner, owner) == 0 &&
        strcmp(g_alias_pid_cache[i].name, name) == 0)
    {
      g_alias_pid_cache[i].pid = pid;
      return true;
    }
  }

  struct alias_pid_cache_entry *new_cache = realloc(g_alias_pid_cache,
                                                    sizeof(struct alias_pid_cache_entry) * (size_t)(g_alias_pid_cache_count + 1));
  if (!new_cache)
    return false;

  g_alias_pid_cache = new_cache;
  struct alias_pid_cache_entry *entry = &g_alias_pid_cache[g_alias_pid_cache_count++];
  snprintf(entry->owner, sizeof(entry->owner), "%s", owner);
  snprintf(entry->name, sizeof(entry->name), "%s", name);
  entry->pid = pid;
  return true;
}

static void alias_pid_cache_clear(void)
{
  free(g_alias_pid_cache);
  g_alias_pid_cache = NULL;
  g_alias_pid_cache_count = 0;
}

static bool load_alias_pid_cache_from_file(void)
{
  struct stat st;
  if (stat(ALIAS_PID_CACHE_PATH, &st) != 0)
    return false;

  time_t now_s = time(NULL);
  if (now_s <= 0)
    return false;

  if ((uint64_t)(now_s - st.st_mtime) > (ALIAS_PID_CACHE_TTL_NS / 1000000000ULL))
    return false;

  FILE *cache_file = fopen(ALIAS_PID_CACHE_PATH, "r");
  if (!cache_file)
    return false;

  alias_pid_cache_clear();

  char line[1024];
  while (fgets(line, sizeof(line), cache_file))
  {
    line[strcspn(line, "\r\n")] = '\0';
    if (line[0] == '\0')
      continue;

    char owner[256];
    char name[256];
    pid_t pid = 0;
    bool has_pid = false;
    if (!parse_alias(line,
                     owner,
                     sizeof(owner),
                     name,
                     sizeof(name),
                     &pid,
                     &has_pid))
      continue;

    if (!has_pid || pid <= 0)
      continue;

    alias_pid_cache_upsert(owner, name, pid);
  }

  fclose(cache_file);
  g_alias_pid_cache_last_refresh_ns = monotonic_time_ns();
  return g_alias_pid_cache_count > 0;
}

static void save_alias_pid_cache_to_file(void)
{
  FILE *cache_file = fopen(ALIAS_PID_CACHE_PATH, "w");
  if (!cache_file)
    return;

  for (int i = 0; i < g_alias_pid_cache_count; ++i)
  {
    fprintf(cache_file,
            "%s,%s,%d\n",
            g_alias_pid_cache[i].owner,
            g_alias_pid_cache[i].name,
            (int)g_alias_pid_cache[i].pid);
  }

  fclose(cache_file);
}

static bool owner_names_equivalent(const char *lhs, const char *rhs)
{
  if (!lhs || !rhs)
    return false;

  if (strcmp(lhs, rhs) == 0)
    return true;

  bool lhs_is_cc = (strcmp(lhs, "Control Centre") == 0 || strcmp(lhs, "ControlCenter") == 0);
  bool rhs_is_cc = (strcmp(rhs, "Control Centre") == 0 || strcmp(rhs, "ControlCenter") == 0);
  return lhs_is_cc && rhs_is_cc;
}

static int owner_name_variants(const char *owner,
                               char out_variants[2][256])
{
  if (!owner)
    return 0;

  snprintf(out_variants[0], 256, "%s", owner);
  if (strcmp(owner, "Control Centre") == 0)
  {
    snprintf(out_variants[1], 256, "ControlCenter");
    return 2;
  }

  if (strcmp(owner, "ControlCenter") == 0)
  {
    snprintf(out_variants[1], 256, "Control Centre");
    return 2;
  }

  return 1;
}

static void refresh_alias_pid_cache(bool force)
{
  uint64_t now_ns = monotonic_time_ns();
  if (!force && g_alias_pid_cache_last_refresh_ns > 0 &&
      (now_ns - g_alias_pid_cache_last_refresh_ns) < ALIAS_PID_CACHE_TTL_NS)
  {
    return;
  }

  if (!force && load_alias_pid_cache_from_file())
  {
    return;
  }

  CFArrayRef window_list = CGWindowListCopyWindowInfo(kCGWindowListOptionAll,
                                                      kCGNullWindowID);
  if (!window_list)
    return;

  alias_pid_cache_clear();

  int window_count = CFArrayGetCount(window_list);
  char owner_buffer[256];
  char name_buffer[256];

  for (int i = 0; i < window_count; ++i)
  {
    CFDictionaryRef dictionary = CFArrayGetValueAtIndex(window_list, i);
    if (!dictionary)
      continue;

    CFStringRef owner_ref = CFDictionaryGetValue(dictionary, kCGWindowOwnerName);
    CFStringRef name_ref = CFDictionaryGetValue(dictionary, kCGWindowName);
    CFNumberRef owner_pid_ref = CFDictionaryGetValue(dictionary, kCGWindowOwnerPID);
    CFNumberRef layer_ref = CFDictionaryGetValue(dictionary, kCGWindowLayer);
    if (!owner_ref || !name_ref || !owner_pid_ref || !layer_ref)
      continue;

    long long int layer = 0;
    CFNumberGetValue(layer_ref, CFNumberGetType(layer_ref), &layer);
    if (layer != 0x19)
      continue;

    if (!CFStringGetCString(owner_ref,
                            owner_buffer,
                            sizeof(owner_buffer),
                            kCFStringEncodingUTF8))
      continue;
    if (strcmp(owner_buffer, "Window Server") == 0)
      continue;

    if (!CFStringGetCString(name_ref,
                            name_buffer,
                            sizeof(name_buffer),
                            kCFStringEncodingUTF8))
      continue;
    if (strcmp(name_buffer, "") == 0)
      continue;

    uint64_t owner_pid = 0;
    CFNumberGetValue(owner_pid_ref,
                     CFNumberGetType(owner_pid_ref),
                     &owner_pid);
    if (owner_pid == 0)
      continue;

    alias_pid_cache_upsert(owner_buffer, name_buffer, (pid_t)owner_pid);
  }

  g_alias_pid_cache_last_refresh_ns = now_ns;
  save_alias_pid_cache_to_file();
  CFRelease(window_list);
}

static bool find_pid_for_alias(const char *owner,
                               const char *name,
                               pid_t *pid_out)
{
  if (!owner || !name || !pid_out)
    return false;

  refresh_alias_pid_cache(false);
  for (int i = 0; i < g_alias_pid_cache_count; ++i)
  {
    if (owner_names_equivalent(g_alias_pid_cache[i].owner, owner) &&
        strcmp(g_alias_pid_cache[i].name, name) == 0)
    {
      *pid_out = g_alias_pid_cache[i].pid;
      return true;
    }
  }

  refresh_alias_pid_cache(true);
  for (int i = 0; i < g_alias_pid_cache_count; ++i)
  {
    if (owner_names_equivalent(g_alias_pid_cache[i].owner, owner) &&
        strcmp(g_alias_pid_cache[i].name, name) == 0)
    {
      *pid_out = g_alias_pid_cache[i].pid;
      return true;
    }
  }

  return false;
}

static bool cfstring_equals_cstr(CFStringRef value, const char *expected)
{
  if (!value || !expected)
  {
    return false;
  }

  char buffer[256] = "";
  if (!CFStringGetCString(value, buffer, sizeof(buffer), kCFStringEncodingUTF8))
  {
    return false;
  }

  return strcmp(buffer, expected) == 0;
}

static bool find_menu_window_bounds_for_alias(const char *alias,
                                              CGRect *out_bounds)
{
  CFArrayRef window_list = CGWindowListCopyWindowInfo(kCGWindowListOptionAll,
                                                      kCGNullWindowID);
  if (!window_list)
  {
    return false;
  }

  bool found = false;
  CGRect exact_bounds = CGRectNull;
  int window_count = CFArrayGetCount(window_list);
  char owner_buffer[256];
  char name_buffer[256];
  char alias_buffer[640];
  for (int i = 0; i < window_count; ++i)
  {
    CFDictionaryRef dictionary = CFArrayGetValueAtIndex(window_list, i);
    if (!dictionary)
      continue;

    CFStringRef owner_ref = CFDictionaryGetValue(dictionary, kCGWindowOwnerName);
    CFStringRef name_ref = CFDictionaryGetValue(dictionary, kCGWindowName);
    CFNumberRef owner_pid_ref = CFDictionaryGetValue(dictionary, kCGWindowOwnerPID);
    CFNumberRef layer_ref = CFDictionaryGetValue(dictionary, kCGWindowLayer);
    CFDictionaryRef bounds_ref = CFDictionaryGetValue(dictionary, kCGWindowBounds);
    if (!owner_ref || !name_ref || !owner_pid_ref || !layer_ref || !bounds_ref)
      continue;

    long long int layer = 0;
    CFNumberGetValue(layer_ref, CFNumberGetType(layer_ref), &layer);
    if (layer != 0x19)
      continue;

    uint64_t owner_pid = 0;
    CFNumberGetValue(owner_pid_ref, CFNumberGetType(owner_pid_ref), &owner_pid);
    if (!CFStringGetCString(owner_ref,
                            owner_buffer,
                            sizeof(owner_buffer),
                            kCFStringEncodingUTF8))
      continue;
    if (!CFStringGetCString(name_ref,
                            name_buffer,
                            sizeof(name_buffer),
                            kCFStringEncodingUTF8))
      continue;

    snprintf(alias_buffer,
             sizeof(alias_buffer),
             "%s,%s,%d",
             owner_buffer,
             name_buffer,
             (int)owner_pid);
    if (strcmp(alias_buffer, alias) != 0)
      continue;

    CGRect bounds = CGRectNull;
    if (!CGRectMakeWithDictionaryRepresentation(bounds_ref, &bounds))
      continue;

    exact_bounds = bounds;
    found = true;
    break;
  }

  if (found && out_bounds)
  {
    *out_bounds = exact_bounds;
  }

  CFRelease(window_list);
  return found;
}

static bool parse_numeric_menu_id(const char *input, int *id_out)
{
  if (!input || !*input)
  {
    return false;
  }

  errno = 0;
  char *endptr = NULL;
  long value = strtol(input, &endptr, 10);
  if (errno != 0 || endptr == input || *endptr != '\0')
  {
    return false;
  }

  if (value < 0 || value > INT_MAX)
  {
    return false;
  }

  if (id_out)
    *id_out = (int)value;
  return true;
}

struct menu_window_entry
{
  double x_center;
  bool owner_matches;
  bool name_matches;
};

static int compare_menu_window_entry_desc(const void *lhs, const void *rhs)
{
  const struct menu_window_entry *a = (const struct menu_window_entry *)lhs;
  const struct menu_window_entry *b = (const struct menu_window_entry *)rhs;
  if (a->x_center < b->x_center)
    return 1;
  if (a->x_center > b->x_center)
    return -1;
  return 0;
}

static bool find_menu_window_ordinal_for_alias(const char *owner,
                                               const char *name,
                                               pid_t pid,
                                               int *ordinal_out)
{
  CFArrayRef window_list = CGWindowListCopyWindowInfo(kCGWindowListOptionAll,
                                                      kCGNullWindowID);
  if (!window_list)
    return false;

  int window_count = CFArrayGetCount(window_list);
  struct menu_window_entry *entries = calloc((size_t)window_count,
                                             sizeof(struct menu_window_entry));
  if (!entries)
  {
    CFRelease(window_list);
    return false;
  }

  int entry_count = 0;
  char owner_buffer[256];
  char name_buffer[256];
  char alias_buffer[640];
  char requested_alias[640];
  snprintf(requested_alias,
           sizeof(requested_alias),
           "%s,%s,%d",
           owner,
           name,
           (int)pid);
  for (int i = 0; i < window_count; ++i)
  {
    CFDictionaryRef dictionary = CFArrayGetValueAtIndex(window_list, i);
    if (!dictionary)
      continue;

    CFStringRef owner_ref = CFDictionaryGetValue(dictionary, kCGWindowOwnerName);
    CFStringRef name_ref = CFDictionaryGetValue(dictionary, kCGWindowName);
    CFNumberRef owner_pid_ref = CFDictionaryGetValue(dictionary, kCGWindowOwnerPID);
    CFNumberRef layer_ref = CFDictionaryGetValue(dictionary, kCGWindowLayer);
    CFDictionaryRef bounds_ref = CFDictionaryGetValue(dictionary, kCGWindowBounds);
    if (!owner_ref || !name_ref || !owner_pid_ref || !layer_ref || !bounds_ref)
      continue;

    long long int layer = 0;
    CFNumberGetValue(layer_ref, CFNumberGetType(layer_ref), &layer);
    if (layer != 0x19)
      continue;

    uint64_t owner_pid = 0;
    CFNumberGetValue(owner_pid_ref, CFNumberGetType(owner_pid_ref), &owner_pid);
    if ((pid_t)owner_pid != pid)
      continue;

    CGRect bounds = CGRectNull;
    if (!CGRectMakeWithDictionaryRepresentation(bounds_ref, &bounds))
      continue;

    struct menu_window_entry entry;
    entry.x_center = rect_center(bounds).x;
    if (!CFStringGetCString(owner_ref,
                            owner_buffer,
                            sizeof(owner_buffer),
                            kCFStringEncodingUTF8))
      continue;
    if (!CFStringGetCString(name_ref,
                            name_buffer,
                            sizeof(name_buffer),
                            kCFStringEncodingUTF8))
      continue;

    snprintf(alias_buffer,
             sizeof(alias_buffer),
             "%s,%s,%d",
             owner_buffer,
             name_buffer,
             (int)owner_pid);

    entry.owner_matches = (strcmp(alias_buffer, requested_alias) == 0);
    entry.name_matches = entry.owner_matches;
    entries[entry_count++] = entry;
  }

  bool found = false;
  if (entry_count > 0)
  {
    qsort(entries,
          (size_t)entry_count,
          sizeof(struct menu_window_entry),
          compare_menu_window_entry_desc);

    for (int i = 0; i < entry_count; ++i)
    {
      if (entries[i].owner_matches && entries[i].name_matches)
      {
        if (ordinal_out)
          *ordinal_out = i;
        found = true;
        break;
      }
    }

    // Keep this strict: no name-only fallback. We only accept exact owner+name.
  }

  free(entries);
  CFRelease(window_list);
  return found;
}

struct ax_child_entry
{
  uint32_t child_index;
  double x_center;
};

static int compare_ax_child_entry_desc(const void *lhs, const void *rhs)
{
  const struct ax_child_entry *a = (const struct ax_child_entry *)lhs;
  const struct ax_child_entry *b = (const struct ax_child_entry *)rhs;
  if (a->x_center < b->x_center)
    return 1;
  if (a->x_center > b->x_center)
    return -1;
  return 0;
}

static pid_t source_pid_for_window(CGRect window_bounds)
{
  if (!source_pid_needs_workaround())
  {
    return 0;
  }

  if (!AXIsProcessTrusted())
  {
    return 0;
  }

  int buffer_bytes = proc_listpids(PROC_ALL_PIDS, 0, NULL, 0);
  if (buffer_bytes <= 0)
  {
    return 0;
  }

  pid_t *pids = calloc((size_t)buffer_bytes, 1);
  if (!pids)
  {
    return 0;
  }

  int bytes_written = proc_listpids(PROC_ALL_PIDS, 0, pids, buffer_bytes);
  int pid_count = bytes_written / (int)sizeof(pid_t);
  CGPoint window_center = rect_center(window_bounds);
  pid_t result = 0;

  for (int i = 0; i < pid_count && result == 0; i++)
  {
    pid_t pid = pids[i];
    if (pid <= 0)
      continue;

    AXUIElementRef app = AXUIElementCreateApplication(pid);
    if (!app)
      continue;

    CFTypeRef extras_ref = NULL;
    AXError error = AXUIElementCopyAttributeValue(app,
                                                  kAXExtrasMenuBarAttribute,
                                                  &extras_ref);
    if (error != kAXErrorSuccess || !extras_ref)
    {
      CFRelease(app);
      continue;
    }

    CFArrayRef children_ref = NULL;
    error = AXUIElementCopyAttributeValue(extras_ref,
                                          kAXVisibleChildrenAttribute,
                                          (CFTypeRef *)&children_ref);
    if (error != kAXErrorSuccess || !children_ref)
    {
      CFRelease(extras_ref);
      CFRelease(app);
      continue;
    }

    CFIndex child_count = CFArrayGetCount(children_ref);
    for (CFIndex j = 0; j < child_count; j++)
    {
      AXUIElementRef child = CFArrayGetValueAtIndex(children_ref, j);
      CGRect child_frame = CGRectNull;
      if (!copy_ax_frame(child, &child_frame))
        continue;

      if (point_distance_squared(window_center, rect_center(child_frame)) <= 1.0)
      {
        result = pid;
        break;
      }
    }

    CFRelease(children_ref);
    CFRelease(extras_ref);
    CFRelease(app);
  }

  free(pids);
  return result;
}

static char *source_name_for_window(CGRect window_bounds)
{
  pid_t source_pid = source_pid_for_window(window_bounds);
  if (source_pid == 0)
  {
    return NULL;
  }

  return copy_process_name(source_pid);
}

void ax_init()
{
  const void *keys[] = {kAXTrustedCheckOptionPrompt};
  const void *values[] = {kCFBooleanTrue};

  CFDictionaryRef options;
  options = CFDictionaryCreate(kCFAllocatorDefault,
                               keys,
                               values,
                               sizeof(keys) / sizeof(*keys),
                               &kCFCopyStringDictionaryKeyCallBacks,
                               &kCFTypeDictionaryValueCallBacks);

  bool trusted = AXIsProcessTrustedWithOptions(options);
  CFRelease(options);
  if (!trusted)
    exit(1);
}

void ax_perform_click(AXUIElementRef element)
{
  if (!element)
    return;
  AXUIElementPerformAction(element, kAXCancelAction);
  usleep(1000);
  AXUIElementPerformAction(element, kAXPressAction);
}

CFStringRef ax_get_title(AXUIElementRef element)
{
  CFTypeRef title = NULL;
  AXError error = AXUIElementCopyAttributeValue(element,
                                                kAXTitleAttribute,
                                                &title);

  if (error != kAXErrorSuccess)
    return NULL;
  return title;
}

void ax_select_menu_option(AXUIElementRef app, int id)
{
  AXUIElementRef menubars_ref = NULL;
  CFArrayRef children_ref = NULL;

  AXError error = AXUIElementCopyAttributeValue(app,
                                                kAXMenuBarAttribute,
                                                (CFTypeRef *)&menubars_ref);
  if (error == kAXErrorSuccess)
  {
    error = AXUIElementCopyAttributeValue(menubars_ref,
                                          kAXVisibleChildrenAttribute,
                                          (CFTypeRef *)&children_ref);

    if (error == kAXErrorSuccess)
    {
      uint32_t count = CFArrayGetCount(children_ref);
      if (id < count)
      {
        AXUIElementRef item = CFArrayGetValueAtIndex(children_ref, id);
        ax_perform_click(item);
      }
      if (children_ref)
        CFRelease(children_ref);
    }
    if (menubars_ref)
      CFRelease(menubars_ref);
  }
}

void ax_print_menu_options(AXUIElementRef app)
{
  AXUIElementRef menubars_ref = NULL;
  CFTypeRef menubar = NULL;
  CFArrayRef children_ref = NULL;

  AXError error = AXUIElementCopyAttributeValue(app,
                                                kAXMenuBarAttribute,
                                                (CFTypeRef *)&menubars_ref);
  if (error == kAXErrorSuccess)
  {
    error = AXUIElementCopyAttributeValue(menubars_ref,
                                          kAXVisibleChildrenAttribute,
                                          (CFTypeRef *)&children_ref);

    if (error == kAXErrorSuccess)
    {
      uint32_t count = CFArrayGetCount(children_ref);

      for (int i = 1; i < count; i++)
      {
        AXUIElementRef item = CFArrayGetValueAtIndex(children_ref, i);
        CFTypeRef title = ax_get_title(item);

        if (title)
        {
          uint32_t buffer_len = 2 * CFStringGetLength(title);
          char buffer[2 * CFStringGetLength(title)];
          CFStringGetCString(title, buffer, buffer_len, kCFStringEncodingUTF8);
          printf("%s\n", buffer);
          CFRelease(title);
        }
      }
    }
    if (menubars_ref)
      CFRelease(menubars_ref);
    if (children_ref)
      CFRelease(children_ref);
  }
}

void ax_print_menu_aliases(void)
{
  if (!CGRequestScreenCaptureAccess())
  {
    printf("[!] Query (-A): Screen Recording permission not granted.\n");
    return;
  }

  CFArrayRef window_list = CGWindowListCopyWindowInfo(kCGWindowListOptionAll,
                                                      kCGNullWindowID);
  if (!window_list)
  {
    printf("[!] Query (-A): Unable to read window list.\n");
    return;
  }

  int window_count = CFArrayGetCount(window_list);
  char owner_buffer[256];
  char name_buffer[256];
  int aliases_printed = 0;
  bool use_source_pid_workaround = source_pid_needs_workaround();

  for (int i = 0; i < window_count; ++i)
  {
    CFDictionaryRef dictionary = CFArrayGetValueAtIndex(window_list, i);
    if (!dictionary)
      continue;

    CFStringRef owner_ref = CFDictionaryGetValue(dictionary, kCGWindowOwnerName);
    CFStringRef name_ref = CFDictionaryGetValue(dictionary, kCGWindowName);
    CFNumberRef owner_pid_ref = CFDictionaryGetValue(dictionary, kCGWindowOwnerPID);
    CFNumberRef layer_ref = CFDictionaryGetValue(dictionary, kCGWindowLayer);
    CFDictionaryRef bounds_ref = CFDictionaryGetValue(dictionary, kCGWindowBounds);

    if (!owner_ref || !name_ref || !owner_pid_ref || !layer_ref || !bounds_ref)
      continue;

    long long int layer = 0;
    CFNumberGetValue(layer_ref, CFNumberGetType(layer_ref), &layer);
    if (layer != 0x19)
      continue;

    uint64_t owner_pid = 0;
    CFNumberGetValue(owner_pid_ref,
                     CFNumberGetType(owner_pid_ref),
                     &owner_pid);

    CFStringGetCString(owner_ref,
                       owner_buffer,
                       sizeof(owner_buffer),
                       kCFStringEncodingUTF8);
    if (strcmp(owner_buffer, "Window Server") == 0)
      continue;

    CFStringGetCString(name_ref,
                       name_buffer,
                       sizeof(name_buffer),
                       kCFStringEncodingUTF8);
    if (strcmp(name_buffer, "") == 0)
      continue;

    const char *print_owner = owner_buffer;
    char *resolved_owner = NULL;
    pid_t print_pid = (pid_t)owner_pid;

    if (use_source_pid_workaround &&
        (strcmp(owner_buffer, "Control Centre") == 0 ||
         strcmp(owner_buffer, "Control Center") == 0))
    {
      CGRect bounds = CGRectNull;
      if (CGRectMakeWithDictionaryRepresentation(bounds_ref, &bounds))
      {
        resolved_owner = source_name_for_window(bounds);
        if (resolved_owner)
          print_owner = resolved_owner;

        pid_t resolved_pid = source_pid_for_window(bounds);
        if (resolved_pid > 0)
          print_pid = resolved_pid;
      }
    }

    printf("%s,%s,%d\n", print_owner, name_buffer, (int)print_pid);
    aliases_printed++;

    if (resolved_owner)
      free(resolved_owner);
  }

  if (aliases_printed == 0)
  {
    printf("[i] Query (-A): No menu aliases found.\n");
  }

  CFRelease(window_list);
}

AXUIElementRef ax_get_extra_menu_item(char *alias)
{
  char requested_owner[256];
  char requested_name[256];
  pid_t requested_pid = 0;
  bool has_requested_pid = false;
  if (!parse_alias(alias,
                   requested_owner,
                   sizeof(requested_owner),
                   requested_name,
                   sizeof(requested_name),
                   &requested_pid,
                   &has_requested_pid))
  {
    return NULL;
  }

  if (!has_requested_pid)
  {
    if (!find_pid_for_alias(requested_owner, requested_name, &requested_pid))
    {
      return NULL;
    }
    has_requested_pid = true;
  }

  AXUIElementRef app = AXUIElementCreateApplication(requested_pid);
  if (!app)
  {
    return NULL;
  }

  AXUIElementRef result = NULL;
  CFTypeRef extras = NULL;
  CFArrayRef children_ref = NULL;
  AXError error = AXUIElementCopyAttributeValue(app,
                                                kAXExtrasMenuBarAttribute,
                                                &extras);
  if (error == kAXErrorSuccess)
  {
    error = AXUIElementCopyAttributeValue(extras,
                                          kAXVisibleChildrenAttribute,
                                          (CFTypeRef *)&children_ref);

    if (error == kAXErrorSuccess)
    {
      uint32_t count = CFArrayGetCount(children_ref);
      int parsed_index = -1;
      bool is_indexed_alias = (sscanf(requested_name, "Item-%d", &parsed_index) == 1 ||
                               sscanf(requested_name, "BentoBox-%d", &parsed_index) == 1);

      if (is_indexed_alias)
      {
        for (uint32_t i = 0; i < count; i++)
        {
          AXUIElementRef item = CFArrayGetValueAtIndex(children_ref, i);
          CFTypeRef desc_ref = NULL;
          CFTypeRef title_ref = NULL;
          CFTypeRef identifier_ref = NULL;

          AXUIElementCopyAttributeValue(item, kAXDescriptionAttribute, &desc_ref);
          AXUIElementCopyAttributeValue(item, kAXTitleAttribute, &title_ref);
          AXUIElementCopyAttributeValue(item, kAXIdentifierAttribute, &identifier_ref);

          char desc_buf[256] = "";
          char title_buf[256] = "";
          char identifier_buf[256] = "";

          if (desc_ref)
          {
            CFStringRef s = (CFStringRef)desc_ref;
            CFStringGetCString(s, desc_buf, sizeof(desc_buf), kCFStringEncodingUTF8);
            CFRelease(desc_ref);
          }
          if (title_ref)
          {
            CFStringRef s = (CFStringRef)title_ref;
            CFStringGetCString(s, title_buf, sizeof(title_buf), kCFStringEncodingUTF8);
            CFRelease(title_ref);
          }
          if (identifier_ref)
          {
            CFStringRef s = (CFStringRef)identifier_ref;
            CFStringGetCString(s, identifier_buf, sizeof(identifier_buf), kCFStringEncodingUTF8);
            CFRelease(identifier_ref);
          }

          if ((strlen(identifier_buf) > 0 && strcmp(identifier_buf, requested_name) == 0) ||
              (strlen(title_buf) > 0 && strcmp(title_buf, requested_name) == 0) ||
              (strlen(desc_buf) > 0 && strcmp(desc_buf, requested_name) == 0))
          {
            result = item;
            CFRetain(result);
            goto done;
          }
        }
      }

      for (uint32_t i = 0; i < count; i++)
      {
        AXUIElementRef item = CFArrayGetValueAtIndex(children_ref, i);
        CFTypeRef desc_ref = NULL;
        CFTypeRef title_ref = NULL;
        CFTypeRef identifier_ref = NULL;

        AXUIElementCopyAttributeValue(item, kAXDescriptionAttribute, &desc_ref);
        AXUIElementCopyAttributeValue(item, kAXTitleAttribute, &title_ref);
        AXUIElementCopyAttributeValue(item, kAXIdentifierAttribute, &identifier_ref);

        char desc_buf[256] = "";
        char title_buf[256] = "";
        char identifier_buf[256] = "";

        if (desc_ref)
        {
          CFStringRef s = (CFStringRef)desc_ref;
          CFStringGetCString(s, desc_buf, sizeof(desc_buf), kCFStringEncodingUTF8);
          CFRelease(desc_ref);
        }
        if (title_ref)
        {
          CFStringRef s = (CFStringRef)title_ref;
          CFStringGetCString(s, title_buf, sizeof(title_buf), kCFStringEncodingUTF8);
          CFRelease(title_ref);
        }
        if (identifier_ref)
        {
          CFStringRef s = (CFStringRef)identifier_ref;
          CFStringGetCString(s, identifier_buf, sizeof(identifier_buf), kCFStringEncodingUTF8);
          CFRelease(identifier_ref);
        }

        if ((strlen(identifier_buf) > 0 && strcmp(identifier_buf, requested_name) == 0) ||
            (strlen(title_buf) > 0 && strcmp(title_buf, requested_name) == 0) ||
            (strlen(desc_buf) > 0 && strcmp(desc_buf, requested_name) == 0))
        {
          result = item;
          CFRetain(result);
          break;
        }
      }

      if (!result && count == 1)
      {
        result = CFArrayGetValueAtIndex(children_ref, 0);
        CFRetain(result);
      }

      if (!result)
      {
        // Fallback: align AX extras with the exact menu-bar window geometry.
        // This mirrors SketchyBar behavior better for Tahoe/Control Center aliases.
        char owner_variants[2][256];
        int variant_count = owner_name_variants(requested_owner, owner_variants);
        CGRect requested_window_bounds = CGRectNull;
        bool has_requested_window_bounds = false;
        char requested_alias[640];

        for (int v = 0; v < variant_count; ++v)
        {
          snprintf(requested_alias,
                   sizeof(requested_alias),
                   "%s,%s,%d",
                   owner_variants[v],
                   requested_name,
                   (int)requested_pid);
          if (find_menu_window_bounds_for_alias(requested_alias,
                                                &requested_window_bounds))
          {
            has_requested_window_bounds = true;
            break;
          }
        }

        if (has_requested_window_bounds)
        {
          AXUIElementRef best_item = NULL;
          double best_distance = DBL_MAX;

          for (uint32_t i = 0; i < count; i++)
          {
            AXUIElementRef item = CFArrayGetValueAtIndex(children_ref, i);
            CGRect child_frame = CGRectNull;
            if (!copy_ax_frame(item, &child_frame))
              continue;

            if (is_indexed_alias)
            {
              double x_diff = fabs(child_frame.origin.x - requested_window_bounds.origin.x);
              if (x_diff <= 12.0)
              {
                best_item = item;
                best_distance = x_diff;
                break;
              }
            }
            else
            {
              double distance = point_distance_squared(rect_center(child_frame),
                                                       rect_center(requested_window_bounds));
              if (distance < best_distance)
              {
                best_distance = distance;
                best_item = item;
              }
            }
          }

          if (best_item)
          {
            result = best_item;
            CFRetain(result);
          }
        }
      }
    }
  }

done:

  if (children_ref)
    CFRelease(children_ref);
  if (extras)
    CFRelease(extras);
  CFRelease(app);

  if (!result)
    return NULL;

  return result;
}

extern int SLSMainConnectionID();
extern void SLSSetMenuBarVisibilityOverrideOnDisplay(int cid, int did, bool enabled);
extern void SLSSetMenuBarVisibilityOverrideOnDisplay(int cid, int did, bool enabled);
extern void SLSSetMenuBarInsetAndAlpha(int cid, double u1, double u2, float alpha);
bool ax_select_menu_extra(char *alias)
{
  AXUIElementRef item = ax_get_extra_menu_item(alias);
  if (!item)
    return false;
  SLSSetMenuBarInsetAndAlpha(SLSMainConnectionID(), 0, 1, 0.0);
  SLSSetMenuBarVisibilityOverrideOnDisplay(SLSMainConnectionID(), 0, true);
  SLSSetMenuBarInsetAndAlpha(SLSMainConnectionID(), 0, 1, 0.0);
  ax_perform_click(item);
  SLSSetMenuBarVisibilityOverrideOnDisplay(SLSMainConnectionID(), 0, false);
  SLSSetMenuBarInsetAndAlpha(SLSMainConnectionID(), 0, 1, 1.0);
  CFRelease(item);
  return true;
}

extern void _SLPSGetFrontProcess(ProcessSerialNumber *psn);
extern void SLSGetConnectionIDForPSN(int cid, ProcessSerialNumber *psn, int *cid_out);
extern void SLSConnectionGetPID(int cid, pid_t *pid_out);
AXUIElementRef ax_get_front_app()
{
  ProcessSerialNumber psn;
  _SLPSGetFrontProcess(&psn);
  int target_cid;
  SLSGetConnectionIDForPSN(SLSMainConnectionID(), &psn, &target_cid);

  pid_t pid;
  SLSConnectionGetPID(target_cid, &pid);
  return AXUIElementCreateApplication(pid);
}

int main(int argc, char **argv)
{
  if (argc == 1)
  {
    printf("Usage: %s [-l | -A | -s id/alias ]\n", argv[0]);
    exit(0);
  }

  if (strcmp(argv[1], "-l") == 0)
  {
    ax_init();
    AXUIElementRef app = ax_get_front_app();
    if (!app)
      return 1;
    ax_print_menu_options(app);
    CFRelease(app);
  }
  else if (strcmp(argv[1], "-A") == 0)
  {
    ax_print_menu_aliases();
  }
  else if (argc == 3 && strcmp(argv[1], "-s") == 0)
  {
    ax_init();
    int id = 0;
    if (parse_numeric_menu_id(argv[2], &id))
    {
      AXUIElementRef app = ax_get_front_app();
      if (!app)
        return 1;
      ax_select_menu_option(app, id);
      CFRelease(app);
    }
    else
    {
      if (!ax_select_menu_extra(argv[2]))
      {
        fprintf(stderr, "[!] Select (-s): Could not resolve alias '%s'\n", argv[2]);
        return 2;
      }
    }
  }
  return 0;
}
