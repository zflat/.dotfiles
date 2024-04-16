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

########################################
# Aliases
########################################

# mise
alias be="mise exec --"
