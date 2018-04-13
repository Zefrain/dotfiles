#!/bin/bash

set -e

sync_dir=$(dirname $(dirname $(realpath $0)))

conf_dir="$sync_dir/conf"
emacs_dir="$conf_dir/emacs"
tmux_dir="$conf_dir/tmux"
zsh_dir="$conf_dir/zsh"
aria_dir="$conf_dir/aria2"

exec_dir="$sync_dir/exec"


_init_conf_zsh() {
    ln -snf "$zsh_dir/.zshrc" ~
    ln -snf "$zsh_dir/.zshenv" ~
    ln -snf "$zsh_dir/.oh-my-zsh/plugins/emacs/emacsclient.sh" /usr/local/bin
}

_init_conf_emacs() {
    ln -snf "$emacs_dir/.spacemacs" ~
    ln -snf "$emacs_dir/.spacemacs.d" ~
}

_init_conf_aria2() {
    ln -snf "$aria_dir" ~/.aria2
}

_init_conf_tmux() {
    ln -snf "$tmux_dir/.tmux.conf" ~/
}

_init_conf() {
    _init_conf_zsh
    _init_conf_aria2
    _init_conf_emacs
    _init_conf_tmux
}


_init_exec() {
    sudo ln -sf $exec_dir/YCM-Generator/config_gen.py /usr/local/bin
}

_init_sync() {
    symlinks -d -r $HOME

    _init_conf
    _init_exec
}

_init_sync
