// Pushes Zotero's WebDAV sync password in from a file at startup, so the
// Nextcloud app password never has to be typed into the sync pane by hand.
//
// It only ever WRITES the password into the place Zotero already keeps it (the
// OS keystore, via the WebDAV storage mode's setPassword), so removing this
// plugin leaves a working sync configuration behind rather than breaking it.

function install() {}
function uninstall() {}
function shutdown() {}

async function startup({ id, version }, reason) {
  // A bootstrapped plugin's startup can run before Zotero's own init has
  // finished, and Zotero.Sync does not exist until it has.
  await Zotero.initializationPromise;

  // Unset means enabled; only an explicit false turns it off.
  if (Zotero.Prefs.get("webdavPassword.enable") === false) return;

  // `Zotero.Prefs.get(pref)` resolves under `extensions.zotero.`. The second
  // argument of its `get(pref, global)` signature reads the ROOT branch
  // instead, so passing it here would look up the wrong pref entirely.
  const file =
    Zotero.Prefs.get("webdavPassword.file")
    || PathUtils.join(Zotero.DataDirectory.dir, "webdav-password");

  // Environment wins over the file, for a test or launchd run with nothing on
  // disk. `Services.env` only exists on new enough platforms, hence the guard.
  let pw = "";
  try {
    if (typeof Services !== "undefined" && Services.env) {
      pw = Services.env.get("ZOTERO_WEBDAV_PASSWORD") || "";
    }
  }
  catch (e) {
    Zotero.logError(e);
  }
  const fromEnv = !!pw;

  if (!pw) {
    if (!(await IOUtils.exists(file))) {
      Zotero.debug(`webdav-password: ${file} does not exist, skipping`);
      return;
    }
    pw = (await Zotero.File.getContentsAsync(file)).trim();
  }

  if (!pw) {
    Zotero.debug("webdav-password: no password found, skipping");
    return;
  }

  // setPassword is an INSTANCE method on the WebDAV storage mode and takes the
  // password alone -- it reads `sync.storage.username` itself, and returns
  // without writing if that pref is empty.
  const controller = Zotero.Sync.Runner.getStorageController("webdav");
  await controller.setPassword(pw);

  Zotero.debug(
    `webdav-password: set WebDAV password from ${fromEnv ? "environment" : file}`
  );
}
