# Notes

## General setup

* GPG and SSH Keys and `pass` and `dmenu` and `xclip`
  * Export keys from a different machine with a passphrase
    ```
    gpg --armor --export-secret-keys username@email | gpg --armor --symmetric --cipher-algo AES256 -o myprivatekeys.asc.asc
    ```
  * Import gpg keys protected by the passphrase
    ```
    gpg --decrypt myprivatekeys.asc.asc | gpg --import
    ```
    * Edit the key to set the trust level to ultimate
      ```
      gpg --list-secret-keys
      gpg --edit-key KEY trust quit
      ```
    * If a key is expired it can be renewed with the [expire edit command](https://gist.github.com/krisleech/760213ed287ea9da85521c7c9aac1df0).
  * Sync ssh keys (may require `chmod 600 ~/.ssh/id*`)
    * Add to the ssh-agent
      ```bash
      eval "$(ssh-agent -s)"
      ssh-add ~/.ssh/id_ed25519
      ```
  * Sync `~/.password-store` content
  * Add custom keyboard shortcut to [passmenu](https://git.zx2c4.com/password-store/tree/contrib/dmenu/passmenu) script
    $ dpkg -L pass | grep passmenu
    /usr/share/doc/pass/examples/dmenu/passmenu
* Install `stow` and set up dotfiles
  * For cross-platform "stow" see [dploy](https://github.com/arecarn/dploy)
* Install other tools
  * wmctrl xbindkeys
  * gimp gmic
  * Emacs >= 26 and editing tools
    * (apt-get) editorconfig
    * (apt-get) llvm clang
    * [ripgrep](https://github.com/BurntSushi/ripgrep#installation)
    * clang-format
      * with pip
        ```bash
        pip3 install clang-format==11.1.0.2
        ```
      * with apt
        ```bash
        audo apt install clang-format-11
        ```
      * with asdf
        * See https://github.com/amrox/asdf-clang-tools
        ```bash
        asdf plugin add clang-format https://github.com/amrox/asdf-clang-tools.git
        asdf install clang-format 11
        asdf global clang-format 11
        ```
    ```bash
    sudo snap install emacs --classic
    ln -s $HOME/.dotfiles/emacs.d .emacs.d
    ```

    ```
    systemctl --user enable emacs.service
    ```
  * docker
    * Install and postinstall
      * https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository
      * https://docs.docker.com/engine/install/linux-postinstall/
    * Configure Docker to start on boot with systemd
      ```bash
      sudo systemctl enable docker.service
      sudo systemctl enable containerd.service
      ```
    * Manage Docker as a non-root user
      ```
      sudo groupadd docker
      sudo usermod -aG docker $USER
      ```
    * Credential Store
      ```bash
      mkdir -p ~/.local/bin
      wget -O ~/.local/bin/docker-credential-pass https://github.com/docker/docker-credential-helpers/releases/download/v0.8.1/docker-credential-pass-v0.8.1.linux-amd64
      chmod +x ~/.local/bin/docker-credential-pass
      ```
      * ~/.docker/config.json
        * `~/.docker`
          ```
          mkdir -p ~/.docker
          ```
        * `~/.docker/config.json`
          ```
          {
            "credsStore":"pass"
          }
          ```
    * Limit size of logs
      * `/etc/docker/daemon.json`
        ```
        {
          "log-driver": "json-file",
          "log-opts": {
            "max-size": "10m",
            "max-file": "3"
          }
        }
        ```
  * RawTherapee
    * Install as a flatpak
      * Allow access to files in different partition
        ```
        sudo flatpak override com.rawtherapee.RawTherapee --filesystem=host
        ```
      * Run as flatpak: `flatpak run com.rawtherapee.RawTherapee`
  * asdf
    * Install `asdf` following [instructions](https://asdf-vm.com/guide/getting-started.html)
      ```bash
      asdf plugin add nodejs
      asdf install nodejs latest
      asdf global nodejs latest
      ```
      * Python installed from source for specific versions
        * https://devguide.python.org/getting-started/setup-building/index.html#deps-on-linux
        * https://github.com/pyenv/pyenv/wiki#suggested-build-environment
        ```bash
        sudo apt install \
        libbz2-dev \
        libffi-dev \
        libgdbm-dev \
        libgdbm-dev \
        liblzma-dev \
        liblzma-dev \
        libncurses-dev \
        libreadline-dev \
        libsqlite3-dev \
        libssl-dev \
        lzma \
        lzma-dev \
        zlib1g-dev
        ```
  * Devcontainer CLI
    ```bash
    npm install -g @devcontainers/cli
    ```
  * Browser extensions
    - Privacy Badger
    - uBlock Origin
    - CanvasBlocker
    - [User Agent Switcher and Manager](https://webextension.org/listing/useragent-switcher.html)

## Desktop environment specifics:

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
