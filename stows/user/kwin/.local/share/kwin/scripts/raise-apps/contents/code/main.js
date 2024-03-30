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

// Note: would be nice if this could query xdg-settings get default-web-browser when the function is invoked instead of use a list of browsers
registerShortcut("RaiseApp-Browser", "Bring the browser application to the front", "Ctrl+Alt+B", () => {raiseApplication(["firefox", "brave"]);});

function raiseApplication(names) {
  // See also code in https://github.com/eddy-geek/kwinactivate/blob/master/winactivate.kwinscript
  const clients = workspace.stackingOrder;
  for (var i = 0; i < clients.length; i++) {
    console.log(`${clients[i].caption} ${clients[i].pid}`);
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
  if (window == workspace.activeWindow) {
    window.minimized = true;
  } else {
    workspace.sendClientToScreen(window, workspace.activeScreen);
    window.onAllDesktops = true;
    window.minimized = false;
    workspace.activeWindow = window;
  }
  window.onAllDesktops = windowWasOnAllDesktops;
}
