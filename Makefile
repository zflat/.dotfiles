##########################
# Tasks to set up dotfiles

ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONEY: list
list:
	@echo "Tasks to set up dotfiles. Available actions:"
	@echo "  make install"
	@echo "  make uninstall"
	@echo "  make user - packages relative to the home folder"
	@echo "  make system - packages relative to the system root"
	@echo "  make uninstall-user"
	@echo "  make uninstall-system"

.PHONEY: install
install: user system

.PHONEY: uninstall
uninstall: uninstall-user uninstall-system

###########################################
# User level packages and configs
# These are relative to the home folder

COMMON_DIRS := ${HOME}/.config ${HOME}/.local/bin
$(COMMON_DIRS):
	@mkdir -p @

define run-user-stow =
cd ${ROOT_DIR}stows/user && stow -v --target=${HOME}$(1)$(2)
endef
STOW_USER_TARGETS =

${HOME}/.docker/config.json:
	mkdir -p `dirname $@`
	echo -e "{\n  \"credsStore\": \"pass\"\n}" > $@

${HOME}/.emacs.d: | ${ROOT_DIR}emacs.d
	ln -sf $(firstword $|) $@

STOW_USER_TARGETS += dmenu
$(lastword $(STOW_USER_TARGETS)): | ${HOME}/.config
	$(call run-user-stow, $@)

STOW_USER_TARGETS += git
$(lastword $(STOW_USER_TARGETS)): | ${HOME}/.config
	$(call run-user-stow, $@)

STOW_USER_TARGETS += gnupg
$(lastword $(STOW_USER_TARGETS)):
	mkdir -p ${HOME}/.$@ # prevents 1 level of tree folding
	find ~/.gnupg -type f -exec chmod 600 {} \;
	find ~/.gnupg -type d -exec chmod 700 {} \;
	$(call run-user-stow, $@)

STOW_USER_TARGETS += vagrant.d
$(lastword $(STOW_USER_TARGETS)):
	mkdir -p ${HOME}/.$@ # prevents 1 level of tree folding
	$(call run-user-stow, $@)

STOW_USER_TARGETS += vscode
$(lastword $(STOW_USER_TARGETS)): | ${HOME}/.config
	$(call run-user-stow, $@)

STOW_USER_TARGETS += xbindkeys
$(lastword $(STOW_USER_TARGETS)): | ${HOME}/.local/bin
	$(call run-user-stow, $@)

.PHONEY: $(STOW_USER_TARGETS)

UNSTOW_USER_TARGETS = $(foreach target,$(STOW_USER_TARGETS),unstow-user-$(target))
.PHONEY: $(UNSTOW_USER_TARGETS)
$(UNSTOW_USER_TARGETS):
	$(call run-user-stow, -D, $(subst unstow-user-,,$@))

.PHONEY: user
user: $(STOW_USER_TARGETS) ${HOME}/.emacs.d ${HOME}/.docker/config.json

.PHONEY: uninstall-user
uninstall-user: $(UNSTOW_USER_TARGETS)
	[ -L ${HOME}/.emacs.d ] && rm -f ${HOME}/.emacs.d

###########################################
# System level packages and configs
# These are relative to the file sytem root

STOW_SYSTEM_TARGETS = docker-system evdev xkb
.PHONEY: $(STOW_SYSTEM_TARGETS)
$(STOW_SYSTEM_TARGETS):
	cd ${ROOT_DIR}stows/system && sudo stow -v --target=/ $@

UNSTOW_SYSTEM_TARGETS = $(foreach target,$(STOW_SYSTEM_TARGETS),unstow-system-$(target))
.PHONEY: $(UNSTOW_SYSTEM_TARGETS)
$(UNSTOW_SYSTEM_TARGETS):
	cd ${ROOT_DIR}stows/system && sudo stow -v --target=/ -D $(subst unstow-system-,,$@)

.PHONEY: xkb
xkb-edits: xkb
	grep modremap /usr/share/X11/xkb/symbols/us || sudo sed --in-place=.old \
	  's/xkb_symbols "basic" {/xkb_symbols "basic" {\n\n    include "modremap(mods-cstgr)"/' \
	  /usr/share/X11/xkb/symbols/us
.PHONEY: restore-xkb-edits
restore-xkb-edits:
	sudo mv /usr/share/X11/xkb/symbols/us.old /usr/share/X11/xkb/symbols/us

.PHONEY: system
system: $(STOW_SYSTEM_TARGETS) xkb-edits

.PHONEY: uninstall-system
uninstall-system: $(UNSTOW_SYSTEM_TARGETS) restore-xkb-edits
