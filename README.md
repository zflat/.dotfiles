.dotfiles
========

Load and install "dotfiles"
----------------------

 * Prerequisites
  * Python 2.7
  * Emacs24
    * ack
    * ag (silversearcher-ag)
  * Cask http://cask.readthedocs.org/
 * Clone the repo into .dotfiles
 * Run the [dotbot] setup script

        .dotfiles/install

 * Re-run the setup script after any changes to install.config.yaml
  * Changes are idempotent so running multiple times is safe

Dotbot
------

Linking and configuration done automatically with [dotbot].

Upgrade dotbot:
`git submodule update --remote dotbot`

Upgrade submodules to their latest versions:
`git submodule update --init --remote`.


License
-------

This software is hereby released into the public domain. That means you can do
whatever you want with it without restriction.

[dotbot]: https://github.com/anishathalye/dotbot