

* GPG and SSH Keys and `pass` and `dmenu` and `xclip`
  * Import gpg keys, ssh keys
  * Sync ssh keys (may require `chmod 600 ~/.ssh/id*`)
  * Sync ~/.password-store content
  * Add custom keyboard shortcut to `passmenu` script
* Install initial software (need to update that list)
* Set up dotfiles
  * Auto setup
  * Manual setup for key mappings
    * dual-function-keys
    * xkb customizations
    * xbindkeys. Add run_xbindkeys.sh as autostart login script
* Install other tools
  * asdf
  * gimp and gmic
  * docker, docker-compose
    ```bash
    systemctl start docker.service
    systemctl enable docker.service
    sudo usermod -aG docker $USER
    ```
* KDE wallet for ssh agent (and gpg agent?)
  * https://forum.manjaro.org/t/howto-use-kwallet-as-a-login-keychain-for-storing-ssh-key-passphrases-on-kde/7088
  * https://forum.manjaro.org/t/configuring-ssh-agent-to-autostart-and-automatically-add-ssh-keys-to-it/99715/2
