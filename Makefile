##########################
# Tasks to set up dotfiles

ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

# Print all documented build targets. A target is documented by adding a
# `## description` comment on its rule line; keep this the single source of
# truth for what each target does.
# See https://embeddedartistry.com/blog/2026/07/24/adding-a-makefile-help-target/
.PHONY: help
help: ## Show this help
	@echo "Tasks to set up dotfiles."
	@echo
	@echo "Usage: make [TASK]"
	@echo
	@echo "Primary tasks:"
	@grep -hE '^[a-zA-Z0-9_-]+:.*?#1# .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?#1# "}; {printf "  %-20s %s\n", $$1, $$2}'
	@echo
	@echo "  Install tasks:"
	@grep -hE '^[a-zA-Z0-9_-]+:.*?#2# .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?#2# "}; {printf "    %-18s %s\n", $$1, $$2}'
	@echo
	@echo "  Uninstall tasks:"
	@grep -hE '^[a-zA-Z0-9_-]+:.*?#3# .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?#3# "}; {printf "    %-18s %s\n", $$1, $$2}'
	@echo
	@echo "Sub-tasks:"
	@grep -hE '^[a-zA-Z0-9_-]+:.*?#4# .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?#4# "}; {printf "  %-20s %s\n", $$1, $$2}'

.PHONEY: install
install: user system #1# Install user and system packages

.PHONEY: uninstall
uninstall: uninstall-user uninstall-system #1# Remove previously installed user and system packages

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

STOW_USER_TARGETS += bash
$(lastword $(STOW_USER_TARGETS)): | ${HOME}/.config
	$(call run-user-stow, $@)
	@echo "Bash customization is set up in '.bash_aliases' which needs to be sourced from the '.bashrc' file."

STOW_USER_TARGETS += zsh
$(lastword $(STOW_USER_TARGETS)): | ${HOME}/.config
	$(call run-user-stow, $@)
	@echo "Zsh customization is set up in '.zshrc_contrib' which needs to be sourced from the '.zshrc' file."

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
user: $(STOW_USER_TARGETS) ${HOME}/.emacs.d ${HOME}/.docker/config.json #2# Install configs in the user HOME directory

.PHONEY: uninstall-user
uninstall-user: $(UNSTOW_USER_TARGETS) #3# Unstow configs previously stowed in the user HOME directory
	[ -L ${HOME}/.emacs.d ] && rm -f ${HOME}/.emacs.d

###########################################
# System level packages and configs
# These are relative to the file sytem root

STOW_SYSTEM_TARGETS = docker-system evdev
.PHONEY: $(STOW_SYSTEM_TARGETS)
$(STOW_SYSTEM_TARGETS):
	cd ${ROOT_DIR}stows/system && sudo stow -v --target=/ $@

# Since /usr/share/X11/xkb may be a symlink managed by package manager
# (Arch linux) do a deeper stow to within the xkb package location
.PHONEY: xkb
xkb: #4# Install xcb symbols file under /user/share/X11/xkb/symbols
	cd ${ROOT_DIR}stows/system && sudo stow -v --target=/usr/share/X11/xkb/symbols $@-symbols

UNSTOW_SYSTEM_TARGETS = $(foreach target,$(STOW_SYSTEM_TARGETS),unstow-system-$(target))
.PHONEY: $(UNSTOW_SYSTEM_TARGETS)
$(UNSTOW_SYSTEM_TARGETS):
	cd ${ROOT_DIR}stows/system && sudo stow -v --target=/ -D $(subst unstow-system-,,$@)

.PHONEY: xkb
xkb-edits: xkb #4# xcb customizations to the US symbols file
	grep modremap /usr/share/X11/xkb/symbols/us || sudo sed --in-place=.old \
	  's/xkb_symbols "basic" {/xkb_symbols "basic" {\n\n    include "modremap(mods-cstgr)"/' \
	  /usr/share/X11/xkb/symbols/us
	setxkbmap -layout us
# Load layout in KDE wayland
# Settings > Keyboard > Layouts > Add English (us), Enable ... Apply
# CLI:
# - https://askubuntu.com/a/1510142
# - https://discuss.kde.org/t/re-read-configuration-after-setting-change/15008/2
#   `dbus-send --session --type=signal --reply-timeout=100 --dest=org.kde.keyboard /Layouts org.kde.keyboard.reloadConfig`


.PHONEY: restore-xkb-edits
restore-xkb-edits: #4# Reverse xcb customizations to the US symbols file
	sudo mv /usr/share/X11/xkb/symbols/us.old /usr/share/X11/xkb/symbols/us

.PHONEY: system
system: $(STOW_SYSTEM_TARGETS) xkb-edits #2# Install system configuration packages relative to the system root

.PHONEY: uninstall-system
uninstall-system: $(UNSTOW_SYSTEM_TARGETS) restore-xkb-edits #3# Restore previously set up system configuration
