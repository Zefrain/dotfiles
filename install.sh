#!/bin/bash

set -e

dotfiles_dir=$(dirname $(realpath $0))

conf_dir="$dotfiles_dir/conf"
emacs_dir="$conf_dir/emacs"
tmux_dir="$conf_dir/tmux"
zsh_dir="$conf_dir/zsh"
aria_dir="$conf_dir/aria2"
sh_dir="$dotfiles_dir/sh"
systemd_dir="$dotfiles_dir/systemd"


init_sh() {
    sudo stow -d $dotfiles_dir -t /usr/local/bin -R sh
}

init_conf() {
    stow -d $conf_dir -t $HOME -R emacs
    stow -d $conf_dir -t $HOME -R zsh
    stow -d $conf_dir -t $HOME -R aria2
    stow -d $conf_dir -t $HOME -R tmux
}


init_dotfiles() {
    git submodule update --init --recursive --force --remote

    if [[ "$(uname -s)" == "Darwin" ]]; then
        brew tap mycli
    fi
install_systemd() {
	stow -d $dotfiles_dir -t $HOME/.config systemd
	cd $systemd_dir;
	for service in $(ls *); do
		systemctl --user start $service
		systemctl --user enable $service
	done
}

darwin_specified() {
	brew tap mycli
}

linux_specified() {
	install_systemd
}

system_specified() {
	PLATFORM=$(sh /usr/local/bin/systype.sh)

	if [ "${PLATFORM}" = "linux" ]; then
		linux_specified
	fi

	if [ "${PLATFORM}" = "Darwin" ]; then
		darwin_specified
	fi
}
    symlinks -d $HOME

    init_sh
    init_conf

	system_specified
}


init_dotfiles
