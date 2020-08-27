.dotfiles
========

Load and install "dotfiles"
----------------------

 * Prerequisites
  * Python 2.7
  * (apt-get) wmctrl
  * (apt-get) xbindkeys
  * Emacs26
    * "Hack" font
    * (apt-get) ack
    * ag (apt-get silversearcher-ag)
    * ripgrep
    * (apt-get) editorconfig
    * [GNU Global with ctags]
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


See Also
--------

Stow: http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html


License
-------

This software is hereby released into the public domain. That means you can do
whatever you want with it without restriction.

[dotbot]: https://github.com/anishathalye/dotbot
[GNU Global with ctags]: https://github.com/leoliu/ggtags/wiki/Install-Global-with-support-for-exuberant-ctags
