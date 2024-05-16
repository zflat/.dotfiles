/**
 * Script that will install a keyboard shortcut to raise the corresponding open application
 *
 * This is an alternative to using wmctrl
 *
 * Install:
 * kpackagetool6 --type=KWin/Script -i ./raise-apps
 *
 * View logs (needs export QT_LOGGING_RULES="kwin_*.debug=true" in profile and re-login):
 * journalctl -f QT_CATEGORY=js QT_CATEGORY=kwin_scripting
 *
 * If no longer using this script, clean up previously added shortcuts with the command:
 * qdbus org.kde.kglobalaccel /component/kwin org.kde.kglobalaccel.Component.cleanUp
 */


// Support for different KDE KWin API versions by settings up aliases
// to abstract away the API differences.
// (https://discuss.kde.org/t/kwin-scripting-from-5-x-to-6-x-compatible/2905/32)
//
// See previous tagged code for old APIs
// https://github.com/KDE/kwin/blob/v5.27.2/src/scripting/workspace_wrapper.h
// https://github.com/KDE/kwin/blob/v5.27.2/src/window.h
const isKDE6 = typeof workspace.windowList === 'function';
let activeWindow;
let setActiveWindow;
let windowList;
let connectWindowActivated;
if (isKDE6) {
  activeWindow                = () => workspace.activeWindow;
  setActiveWindow             = (window) => { workspace.activeWindow = window; };
  windowList                  = () => workspace.stackingOrder;
  connectWindowActivated      = (handler) => workspace.windowActivated.connect(handler);
} else {
  activeWindow                = () => workspace.activeClient;
  setActiveWindow             = (window) => { workspace.activeClient = window; };
  windowList                  = () => workspace.clientList();
  connectWindowActivated      = (handler) => workspace.clientActivated.connect(handler);
}

// Note: would be nice if this could query xdg-settings get default-web-browser when the function is invoked instead of use a list of browsers
registerShortcut("RaiseApp-Browser", "Bring the browser application to the front", "Ctrl+Alt+B", () => {raiseApplication(["firefox", "brave"]);});

registerShortcut("RaiseApp-Emacs", "Bring the Emacs frame to the front", "Ctrl+Alt+K", () => {raiseApplication(["-emacs-"]);});

registerShortcut("RaiseApp-Terminal", "Bring the terminal application to the front", "Ctrl+Alt+O", () => {raiseApplication(["— konsole"]);});

function raiseApplication(names) {
  // See also code in https://github.com/eddy-geek/kwinactivate/blob/master/winactivate.kwinscript
  const clients = windowList();
  for (var i = 0; i < clients.length; i++) {
    console.log(`[Client ${String(i).padStart(2, "0")} ${clients[i].pid}] ${clients[i].caption}`);
    var is_target = names.findIndex(
      (name) => (clients[i].caption.toLocaleLowerCase('en-US').indexOf(name) >= 0)
    ) >= 0;
    if(is_target) {
      toggle(clients[i]);
      console.info(`Activated client: ${clients[i].active ? "true" : "false"}`);
      return;
    }
  }
  console.info("Client not found");
}

function toggle(window) {
  const windowWasOnAllDesktops = window.onAllDesktops;
  if (window == activeWindow()) {
    window.minimized = true;
  } else {
    if (window.minimized == true) {
      // Only change the screen for the window if it is minimized
      workspace.sendClientToScreen(window, workspace.activeScreen);
    }
    window.onAllDesktops = true;
    window.minimized = false;
    setActiveWindow(window);
  }
  window.onAllDesktops = windowWasOnAllDesktops;
}
