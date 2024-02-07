##########################
# Tasks to set up dotfiles

.PHONEY: list
list:
	@echo "Tasks to set up dotfiles. Available actions:"
	@echo "  make all"
	@echo "  make user - packages relative to the home folder"
	@echo "  make system - packages relative to the system root"

.PHONEY: all
all: user system

###########################################
# User level packages and configs
# These are relative to the home folder

.PHONEY: user
user: git gnupg vagrant vscode xbindkeys ${HOME}/.emacs.d

define run-user-stow
cd stows/user && stow -v --target=${HOME} $@
endef

${HOME}/.config:
	mkdir -p ${HOME}/.config

${HOME}/.local/bin:
	mkdir -p ${HOME}/.local/bin

.PHONEY: git
git: ${HOME}/.config
	$(run-user-stow)

.PHONEY: gnupg
gnupg:
	mkdir -p ${HOME}/.gnupg
	$(run-user-stow)

.PHONEY: vagrant
vagrant:
	mkdir -p ${HOME}/.vagrant.d
	$(run-user-stow)

.PHONEY: vscode
vscode: ${HOME}/.config
	$(run-user-stow)

.PHONEY: xbindkeys
xbindkeys: ${HOME}/.local/bin
	$(run-user-stow)

${HOME}/.emacs.d:
	mkdir -p ${HOME}/.emacs.d

###########################################
# System level packages and configs
# These are relative to the file sytem root

.PHONEY: system
system: evdev xkb-edits

define run-system-stow
cd stows/system && sudo stow -v --target=/ $@
endef

.PHONEY: evdev
evdev:
	$(run-system-stow)

.PHONEY: xkb
xkb:
	$(run-system-stow)
.PHONEY: xkb-edits
xkb-edits: xkb
	grep modremap /usr/share/X11/xkb/symbols/us || sudo sed --in-place=.old \
	  's/xkb_symbols "basic" {/xkb_symbols "basic" {\n\n    include "modremap(mods-cstgr)"/' \
	  /usr/share/X11/xkb/symbols/us
