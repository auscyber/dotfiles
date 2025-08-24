# Definition Document for Keybind Program

- Inspired by which-key, but with a focus on keybinds for various applications and window managers.

- Aims to provide a structured way to define, display, and manage keybinds across different platforms and applications.
- Supports both global keybinds and application-specific keybinds.

- Should be able to generate configuration files for different applications and window managers based on the defined keybinds.
- Provides a user interface to display keybinds, allowing users to hover over each keybind to see its function.
- Supports nested groups for organizing keybinds by application or functionality.


# Format 
- Keybinds are defined in a structured format, allowing for easy parsing and generation of configuration files.
- Supports grouping of keybinds by application or functionality.



- Transformers are defined which convert the structured keybind definitions into specific formats for different applications or window managers.
- Transformers can be used to generate configuration files for applications like Alacritty, Sway, Hyprland, Amethyst, and Skhd.
- Each transformer will have its own set of rules and syntax for defining keybinds, allowing for flexibility in how keybinds are represented across different platforms.
- The program will include a set of predefined transformers for common applications and window managers, but users can also define their own transformers for custom applications or configurations.


## Toplevel Structure

- Here is the schema of what an individual keybind looks like:







# Program


- Takes a config file, with a series of keybinds
- Tagged with what they do

- Grouped by application, or global
- Groups can be nested
- Can be used to generate a config file for a specific application or window manager

- Grouped by functionality, e.g. navigation, window management, etc.

- Produces an app which displays all the binds, hover on each part


# Platforms

## Individual Apps

### Alacritty

### Ghostty

## Global


### Sway

- bindsym <key> <command>

Example:
```plaintext
bindsym $mod+Return exec alacritty
bindsym $mod+Shift+q kill
bindsym $mod+Shift+e exec swaymsg exit
bindsym $mod+Shift+r reload
bindsym $mod+Shift+c reload config
bindsym $mod+Shift+f fullscreen toggle
```


### Hyprland

- bind = MODS, key, dispatcher, params


Example:
```plaintext
bind = $mod, Return, exec, alacritty
bind = $mod, Shift+q, kill
bind = $mod, Shift+e, exec, swaymsg exit
bind = $mod, Shift+r, reload
bind = $mod, Shift+c, reload config
bind = $mod, Shift+f, fullscreen toggle
```


### Amethyst



### Skhd

                     A hotkey is written according to the following rules:

                       hotkey       = <mode> '<' <action> | <action>

                       mode         = 'name of mode' | <mode> ',' <mode>

                       action       = <keysym> '[' <proc_map_lst> ']' | <keysym> '->' '[' <proc_map_lst> ']'
                                      <keysym> ':' <command>          | <keysym> '->' ':' <command>
                                      <keysym> ';' <mode>             | <keysym> '->' ';' <mode>

                       keysym       = <mod> '-' <key> | <key>

                       mod          = 'modifier keyword' | <mod> '+' <mod>

                       key          = <literal> | <keycode>

                       literal      = 'single letter or built-in keyword'

                       keycode      = 'apple keyboard kVK_<Key> values (0x3C)'

                       proc_map_lst = * <proc_map>

                       proc_map     = <string> ':' <command> | <string>     '~' |
                                      '*'      ':' <command> | '*'          '~'

                       string       = '"' 'sequence of characters' '"'

                       command      = command is executed through '$SHELL -c' and
                                      follows valid shell syntax. if the $SHELL environment
                                      variable is not set, it will default to '/bin/bash'.
                                      when bash is used, the ';' delimeter can be specified
                                      to chain commands.

                                      to allow a command to extend into multiple lines,
                                      prepend '\' at the end of the previous line.

                                      an EOL character signifies the end of the bind.

                       ->           = keypress is not consumed by skhd

                       *            = matches every application not specified in <proc_map_lst>

                       ~            = application is unbound and keypress is forwarded per usual, when specified in a <proc_map>

  NOTE(koekeishiya): A mode is declared according to the following rules:

                       mode_decl = '::' <name> '@' ':' <command> | '::' <name> ':' <command> |
                                   '::' <name> '@'               | '::' <name>

                       name      = desired name for this mode,

                       @         = capture keypresses regardless of being bound to an action

                       command   = command is executed through '$SHELL -c' and
                                   follows valid shell syntax. if the $SHELL environment
                                   variable is not set, it will default to '/bin/bash'.
                                   when bash is used, the ';' delimeter can be specified
                                   to chain commands.

                                   to allow a command to extend into multiple lines,
                                   prepend '\' at the end of the previous line.

                                   an EOL character signifies the end of the bind.