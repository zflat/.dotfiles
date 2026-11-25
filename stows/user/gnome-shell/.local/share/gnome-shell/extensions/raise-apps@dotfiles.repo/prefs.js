import Adw from 'gi://Adw';

import { ExtensionPreferences }
    from 'resource:///org/gnome/Shell/Extensions/js/extensions/prefs.js';

export default class RaiseAppsPreferences
    extends ExtensionPreferences {

    fillPreferencesWindow(window) {
        const settings = this.getSettings();

        const page = new Adw.PreferencesPage();
        window.add(page);

        const behavior = new Adw.PreferencesGroup({title: 'Behavior'});
        page.add(behavior);

        const moveSwitch = new Adw.SwitchRow({title: 'Move window to active workspace'});
        settings.bind('move-to-active-workspace', moveSwitch, 'active', 0);
        behavior.add(moveSwitch);

        const apps = new Adw.PreferencesGroup({title: 'Application Matching'});
        page.add(apps);

        this._addClassEditor(
            apps,
            settings,
            'Browser Classes',
            'browser-classes'
        );
        this._addClassEditor(
            apps,
            settings,
            'Emacs Classes',
            'emacs-classes'
        );
        this._addClassEditor(
            apps,
            settings,
            'Terminal Classes',
            'terminal-classes'
        );
    }

    _addClassEditor(
        group,
        settings,
        title,
        key
    ) {
        const row = new Adw.EntryRow({title});
        row.set_text(settings .get_strv(key) .join(','));
        row.connect(
            'notify::text',
            () => {
                settings.set_strv(
                    key,
                    row.get_text()
                        .split(',')
                        .map(v => v.trim())
                        .filter(Boolean)
                );
            }
        );
        group.add(row);
    }
}
