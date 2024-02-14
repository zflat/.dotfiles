##########################
# Tasks to set up dotfiles

ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

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
user: git gnupg vagrant vscode xbindkeys ${HOME}/.emacs.d ${HOME}/.docker/config.json

define run-user-stow
cd stows/user && stow -v --target=${HOME} $@
endef

${HOME}/.config:
	@mkdir -p ${HOME}/.config

${HOME}/.local/bin:
	@mkdir -p ${HOME}/.local/bin

${HOME}/.local/bin/docker-credential-pass: ${HOME}/.local/bin
	wget -O $@ https://github.com/docker/docker-credential-helpers/releases/download/v0.8.1/docker-credential-pass-v0.8.1.linux-amd64
	chmod +x $@

${HOME}/.docker/config.json:
	mkdir -p ${HOME}/.docker
	echo "{\n  \"credsStore\":\"pass\"\n}" >> $@

${HOME}/.emacs.d: emacs.d
	ln -sf ${ROOT_DIR}$< $@

git: ${HOME}/.config
	$(run-user-stow)

gnupg:
	@mkdir -p ${HOME}/.gnupg
	$(run-user-stow)

vagrant:
	@mkdir -p ${HOME}/.vagrant.d
	$(run-user-stow)

vscode: ${HOME}/.config
	$(run-user-stow)

xbindkeys: ${HOME}/.local/bin
	$(run-user-stow)

.PHONEY: git gnupg vagrant vscode xbindkeys

###########################################
# System level packages and configs
# These are relative to the file sytem root

.PHONEY: system
system: docker-system evdev xkb-edits

define run-system-stow
cd stows/system && sudo stow -v --target=/ $@
endef

docker-system:
	$(run-system-stow)

evdev:
	$(run-system-stow)

xkb:
	$(run-system-stow)
xkb-edits: xkb
	grep modremap /usr/share/X11/xkb/symbols/us || sudo sed --in-place=.old \
	  's/xkb_symbols "basic" {/xkb_symbols "basic" {\n\n    include "modremap(mods-cstgr)"/' \
	  /usr/share/X11/xkb/symbols/us

.PHONEY: docker-system evdev xkb xkb-edits
