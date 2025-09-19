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
#
# Takes a positional argument for the docker container suffix
# name. Can be a string with a space to not use a suffix.
function devcontainer-project-full () {
    container_suffix="${1:--dev}"
    container_name="$(basename $(pwd))$(echo ${container_suffix} | tr -d '')"
    if [[ $(docker ps -a --format '{{print .Names}}' | grep -i "^${container_name}$") ]]; then
        XAUTH=$(docker inspect \
                       --format '{{range .Mounts}}{{if (gt (len .Source) (len "xauth_")) }}{{- println .Source }}{{end}}{{end}}' \
                       "${container_name}" | grep -i xauth )
        if [ $? -ne 0 ]; then
            return
        fi
        if [[ "${XAUTH}" != "" ]] && [[ "${XAUTH}" != "${XAUTHORITY}" ]]; then
            touch $XAUTH
            xauth nlist $DISPLAY | sed -e 's/^..../ffff/' | xauth -f $XAUTH nmerge -
        fi
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
