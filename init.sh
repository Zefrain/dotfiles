#!/bin/bash

set -e

dotfiles_dir=$(dirname $(realpath $0))

conf_dir="$dotfiles_dir/conf"
emacs_dir="$conf_dir/emacs"
tmux_dir="$conf_dir/tmux"
zsh_dir="$conf_dir/zsh"
aria_dir="$conf_dir/aria2"

sh_dir="$dotfiles_dir/sh"


init_sh() {
    stow -d $dotfiles_dir -t /usr/local/bin -R sh
}

init_conf() {
    stow -d $conf_dir -t $HOME -R emacs
    stow -d $conf_dir -t $HOME -R zsh
}


init_dotfiles() {
    git submodule update --init --recursive


    symlinks -d $HOME

    init_sh
    init_conf
}


init_dotfiles
