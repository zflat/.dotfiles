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
user: git xbindkeys

define run-user-stow
cd stows/user && stow -v --target=${HOME} $@
endef

.PHONEY: git
git:
	$(run-user-stow)

.PHONEY: xbindkeys
xbindkeys:
	$(run-user-stow)


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
	grep modremap /usr/share/X11/xkb/symbols/us || sudo sed --in-place=-stock \
	  's/xkb_symbols "basic" {/xkb_symbols "basic" {\n\n    include "modremap(mods-cstgr)"/' \
	  /usr/share/X11/xkb/symbols/us
