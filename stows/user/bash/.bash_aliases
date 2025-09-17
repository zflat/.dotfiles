#!/usr/bin/env bash

########################################
# Bash customization to add to a .bashrc
########################################

# Set up common tools and environment variables
if [[ -f "$HOME/.bashrc_contrib" ]]; then
    . "$HOME/.bashrc_contrib"
fi

# Set up tech-stack specific environment
if [[ -f "$HOME/.bashrc_devenv" ]]; then
    . "$HOME/.bashrc_devenv"
fi

#######################################
# Functions used with aliases
#######################################

# Starts and runs a devcontiner and attaches to it
# Can be used with alias like this
# alias diyprojup="cd $HOME/proj/diyproj && devcontainer-project-full"
function devcontainer-project-full () {
    XAUTH=$(docker inspect \
                   --format '{{range .Mounts}}{{if (and (gt (len .Source) 12) (eq (slice .Source 0 11) "/tmp/xauth_")) }}{{ print .Source }}{{end}}{{end}}' \
                   "$(basename $(pwd))-dev")
    if [ $? -ne 0 ]; then
        return
    fi
    if [ "${XAUTH}" != "" ] && ["${XAUTH}" != "${XAUTHORITY}" ]; then
        touch $XAUTH
        xauth nlist $DISPLAY | sed -e 's/^..../ffff/' | xauth -f $XAUTH nmerge -
    fi
    devcontainer up --workspace-folder "$(pwd)"
    devcontainer exec --workspace-folder "$(pwd)" bash
};

# Attaches to an existing devcontainer
function devcontainer-project () {
  devcontainer exec --workspace-folder "$(pwd)" bash
};

########################################
# Aliases
########################################

# mise
alias be="mise exec --"
