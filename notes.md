# Notes

## General setup

* GPG and SSH Keys and `pass` and `dmenu` and `xclip`
  * Import gpg keys
  * Sync ssh keys (may require `chmod 600 ~/.ssh/id*`)
  * Sync `~/.password-store content`
  * Add custom keyboard shortcut to `passmenu` script
* Install `stow` and set up dotfiles
  * For cross-platform "stow" see [dploy](https://github.com/arecarn/dploy)
* Install other tools
  * wmctrl xbindkeys
  * asdf
  * gimp gmic
  * docker
    * Post-instal:
      ```bash
      systemctl start docker.service
      systemctl enable docker.service
      sudo usermod -aG docker $USER
      ```
  * Emacs >= 26 and editing tools
    * (apt-get) editorconfig
    * (apt-get) llvm clang
    * [ripgrep](https://github.com/BurntSushi/ripgrep#installation)
    ```bash
    sudo snap install emacs --classic
    ln -s $HOME/.dotfiles/emacs.d .emacs.d
    ```

## Desktop environment specifics: 

* KDE wallet for ssh agent (and gpg agent?)
  * https://forum.manjaro.org/t/howto-use-kwallet-as-a-login-keychain-for-storing-ssh-key-passphrases-on-kde/7088
  * https://forum.manjaro.org/t/configuring-ssh-agent-to-autostart-and-automatically-add-ssh-keys-to-it/99715/2
