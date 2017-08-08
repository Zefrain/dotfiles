#!/bin/bash

set -e

sync_dir=$(dirname $(dirname $(realpath $0)))

conf_dir="$sync_dir/conf"
emacs_dir="$conf_dir/emacs"
tmux_dir="$conf_dir/tmux"
zsh_dir="$conf_dir/zsh"
aria_dir="$conf_dir/aria2"

scripts_dir="$sync_dir/conf"


_init_conf_zsh() {
    ln -sf "$zsh_dir/.zshrc" ~
    ln -sf "$zsh_dir/.zshenv" ~
}

_init_conf_emacs() {
    ln -sf "$emacs_dir/.spacemacs" ~
    ln -sf "$emacs_dir/.spacemacs.d" ~
}

_init_conf_aria2() {
    ln -sf "$aria_dir" ~/.aria2
}

_init_conf() {
    _init_conf_zsh
    _init_conf_aria2
    _init_conf_emacs
}



_init_script() {
    export PATH=$PATH:$pwd
    ln -sf $scripts_dir/YCM-Generator/config_gen.py .
}

_init_sync() {
    find -L $HOME -type l -exec rm -- {} +

    _init_conf
    _init_script
}

_init_sync
