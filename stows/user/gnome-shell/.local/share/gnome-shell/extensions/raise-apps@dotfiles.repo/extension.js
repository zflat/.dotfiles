import Meta from 'gi://Meta';
import Shell from 'gi://Shell';

import * as Main from 'resource:///org/gnome/shell/ui/main.js';
import { Extension }
from 'resource:///org/gnome/shell/extensions/extension.js';

/**
 * Script that will install a keyboard shortcut to raise the corresponding open application
 *
 * This is an alternative to using wmctrl
 *
 * Installation from source
 *
 * Compile the schema
 * ```
 * glib-compile-schemas ~/.local/share/gnome-shell/extensions/raise-apps@dotfiles.repo/schemas
 * ```
 *
 * Verify
 * ```
 * gnome-extensions info raise-apps@dotfiles.repo
 * ```
 *
 * Enable
 * ```
 * gnome-extensions enable raise-apps@dotfiles.repo
 * ```
 *
 * Discover WM_CLASS Values
 * To determine the correct identifiers for applications, run `xprop WM_CLASS` and click on the window.
 *
 *
 * # Development workflow
 *
 * View journalctl logs:
 * ```
 * journalctl --user -f -o cat /usr/bin/gnome-shell
 * ```
 *
 * Reload the extension after making changes
 * https://gjs.guide/extensions/development/debugging.html
 * ```
 * glib-compile-schemas ~/.local/share/gnome-shell/extensions/raise-apps@dotfiles.repo/schemas &&
 * gnome-extensions reset raise-apps@dotfiles.repo
 * ```
 * Run a nested GNOME Shell for testing
 * ```
 * dbus-run-session -- gnome-shell --nested --wayland
 * ```
 */
export default class RaiseAppsExtension extends Extension {

  enable() {
    this._settings = this.getSettings();
    this._addBinding(
      'raise-browser',
      false,
      'browser-classes'
    );
    this._addBinding(
      'raise-emacs',
      false,
      'emacs-classes'
    );
    this._addBinding(
      'raise-emacs-here',
      true,
      'emacs-classes'
    );
    this._addBinding(
      'raise-terminal',
      false,
      'terminal-classes'
    );
    this._addBinding(
      'raise-terminal-here',
      true,
      'terminal-classes'
    );
  }

  disable() {
    [
      'raise-browser',
      'raise-emacs',
      'raise-emacs-here',
      'raise-terminal',
      'raise-terminal-here'
    ].forEach(binding => {
      Main.wm.removeKeybinding(binding);
    });
    this._settings = null;
  }

  _addBinding(name, moveToWorkspace, classesKey) {
    // addKeybinding signature
    // https://gitlab.gnome.org/GNOME/gnome-shell/-/blob/main/js/ui/windowManager.js?ref_type=heads#L1106
    // https://github.com/gjsify/gnome-shell/blob/6047f4e70c47d630814ea9b25ce040893a6b8797/packages/gnome-shell/src/ui/windowManager.d.ts#L59
    Main.wm.addKeybinding(
      name,
      this._settings,
      Meta.KeyBindingFlags.NONE, // https://gnome.pages.gitlab.gnome.org/mutter/meta/flags.KeyBindingFlags.html
      Shell.ActionMode.NORMAL, // Use `g-ir-inspect Shell` to view API defined at https://gitlab.gnome.org/GNOME/gnome-shell/-/blob/main/src/shell-action-modes.h?ref_type=heads
      () => {
        this._raiseApplication(
          this._settings.get_strv(classesKey),
          moveToWorkspace
        );
      }
    );
  }

  _matchingWindow(window, classNames) {
    const wmClass = window.get_wm_class();
    if (!wmClass) {
      return false;
    }
    const lower = wmClass.toLowerCase();
    return classNames.some(name =>
      lower.includes(name.toLowerCase())
    );
  }

  _raiseApplication(classNames, moveToWorkspace) {
    // Get window in most recently used order and find the most recent match
    const windows = global.display.get_tab_list(Meta.TabList.NORMAL_ALL, null);
    const matches = windows.filter(win => this._matchingWindow(win, classNames));
    if (matches.length === 0) {
      return;
    }

    const focusedWindow = global.display.focus_window;
    let target = matches[0];

    // If currently focused window is one of the matching windows,
    // activate the next most recently used window.
    if (focusedWindow && this._matchingWindow(focusedWindow, classNames)) {
      if (!moveToWorkspace) {
        if (matches.length === 1) {
          focusedWindow.minimize();
          return;
        }
        const currentIndex = matches.indexOf(focusedWindow);
        if (currentIndex >= 0) {
          target = matches[(currentIndex + 1) % matches.length];
        }
      }
    }

    if (target.minimized) {
      target.unminimize();
    }

    if (moveToWorkspace) {
      try {
        const workspace = global.workspace_manager .get_active_workspace();
        target.change_workspace(workspace);
      } catch (e) {
        logError(e);
      }
    }

    // See https://github.com/gjsify/gnome-shell/blob/6047f4e70c47d630814ea9b25ce040893a6b8797/packages/gnome-shell/src/ui/main.d.ts#L191
    Main.activateWindow(target, global.get_current_time());
  }
}
