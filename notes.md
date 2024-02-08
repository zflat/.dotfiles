# Notes

## General setup

* GPG and SSH Keys and `pass` and `dmenu` and `xclip`
  * Export keys from a different machine with a passphrase
    ```
    gpg --armor --export-secret-keys | gpg --armor --symmetric --cipher-algo AES256 -o myprivatekeys.asc.asc
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
    * If a key is expired it can be renewed with the [expire edit command](https://gist.github.com/krisleech/760213ed287ea9da85521c7c9aac1df0)
  * Sync ssh keys (may require `chmod 600 ~/.ssh/id*`)
    * Add to the ssh-agent
      ```bash
      eval "$(ssh-agent -s)"
      ssh-add ~/.ssh/id_ed25519
      ```
  * Sync `~/.password-store content`
  * Add custom keyboard shortcut to `passmenu` script
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
    ```bash
    sudo snap install emacs --classic
    ln -s $HOME/.dotfiles/emacs.d .emacs.d
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
  * asdf
    * Install `asdf` from website
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

## Desktop environment specifics:

* KDE wallet for ssh agent (and gpg agent?)
  * https://forum.manjaro.org/t/howto-use-kwallet-as-a-login-keychain-for-storing-ssh-key-passphrases-on-kde/7088
  * https://forum.manjaro.org/t/configuring-ssh-agent-to-autostart-and-automatically-add-ssh-keys-to-it/99715/2
