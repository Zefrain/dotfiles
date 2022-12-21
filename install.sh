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
	mkdir -p ~/.config/clash
	# stow -d $conf_dir -t ~/.config/clash clash
	stow -d $conf_dir -t $HOME -R emacs
	stow -d $conf_dir -t $HOME -R zsh
	stow -d $conf_dir -t $HOME -R aria2
	stow -d $conf_dir -t $HOME -R tmux
}

init_zsh_custom_post() {
	sed -i -e "s|#\? \?ZSH_CUSTOM=.*|ZSH_CUSTOM=${zsh_dir}\/omz_custom|g" $HOME/.zshrc
}


install_systemd() {
	sudo apt install -y xsel

	stow -d $dotfiles_dir -t $HOME/.config/systemd systemd
	systemctl --user daemon-reload
	cd $systemd_dir
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

do_git() {
	git submodule update --init --recursive --force --remote
}

do_symlinks() {
	symlinks -d $HOME
}

add_vim() {
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	stow -d $conf_dir -t $HOME -R vim
}

init_dotfiles() {
	do_symlinks

	case $1 in 
		init_sh)
			init_sh
			;;

		init_conf)
			init_conf
			;;


		init_zsh_custom_post)
			init_zsh_custom_post
			;;


		system_specified)
			system_specified
			;;

		add_vim)
			add_vim
			;;

		*)
			do_git
			do_symlinks
			init_sh
			init_conf
			init_zsh_custom_post
			add_vim
			system_specified
			;;
	esac


}


init_dotfiles $1
