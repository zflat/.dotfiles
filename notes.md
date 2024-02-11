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
    ```
    asdf plugin add python
    asdf install python 3.12.1
    python -m pip install clang-format==11.1.0.2
    ```
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
* hunspell spell checking
  * Install the hunspell-en_us package
  * Confirm with `hunspell -D`
* Dictionary
  * https://wiki.archlinux.org/title/sdcv
  * https://web.archive.org/web/20200630200122/http://download.huzheng.org/dict.org/
    * [The Collaborative International Dictionary of English](https://web.archive.org/web/20200630200122/http://download.huzheng.org/dict.org/stardict-dictd_www.dict.org_gcide-2.4.2.tar.bz2)
    * [Merrian Webster 10th dictionary](https://web.archive.org/web/20200630200122/http://download.huzheng.org/dict.org/stardict-merrianwebster-2.4.2.tar.bz2)
  * https://aur.archlinux.org/packages/stardict-wordnet
  * QDict
    * http://madman-team.blogspot.com/2015/05/qdict-guide.html
  * Aard2
    * https://github.com/itkach/slob/wiki/Dictionaries
