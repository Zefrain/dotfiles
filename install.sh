#!/bin/bash

set -e

dotfiles_dir=$(dirname $(realpath $0))

conf_dir="$dotfiles_dir/conf"
zsh_dir="$conf_dir/zsh"
systemd_dir="$dotfiles_dir/systemd"


PLATFORM=$(sh /usr/local/bin/systype.sh)
OS_RELEASE=$(lsb_release -d | awk '{print $2}')
init_packages() {
	if [ "$OS_RELEASE" == "Ubuntu" ]; then
		sudo apt update && sudo apt install -y \
			stow \
			ripgrep \
			git \
			exuberant-ctags \
			cscope \
			global \
			clang-format \
			symlinks \
			xclip xsel \
			build-essential cmake vim-nox python3-dev \
			mono-complete golang nodejs default-jdk npm
	fi
}

init_sh() {
	sudo stow -d $dotfiles_dir -t /usr/local/bin -R sh
}

init_conf() {
	mkdir -p ~/.config/clash
	# stow -d $conf_dir -t ~/.config/clash clash
	stow -d $conf_dir -t $HOME -R emacs
	stow -d $conf_dir -t $HOME -R aria2
	stow -d $conf_dir -t $HOME -R tmux
}

init_zsh() {
	rm -f ~/.zshrc
	stow -d $conf_dir -t $HOME -R zsh
	sed -i -e "s|#\? \?ZSH_CUSTOM=.*|ZSH_CUSTOM=${zsh_dir}\/omz_custom|g" $HOME/.zshrc
}

init_systemd() {
	sudo apt install -y xsel

	stow -d . -t $HOME/.config/systemd systemd
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

do_ubuntu_install() {
	sudo apt update
	sudo apt install -y xsel gnutls-bin zsh curl stow symlinks tmux vim symlinks

}

linux_specified() {
	init_systemd
}

system_specified() {
	PLATFORM=$(sh /usr/local/bin/systype.sh)

	if [ "${PLATFORM}" = "linux" ]; then
		git submodule deinit workflows/Ariafred

		linux_specified
	fi

	if [ "${PLATFORM}" = "macos" ]; then
		darwin_specified
	fi
}

init_git() {
	git submodule update --init --recursive --force --remote
}

init_symlinks() {
	symlinks -d $HOME
}

init_vim() {
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	stow -d $conf_dir -t $HOME -R vim
	vim +PlugInstall +qall 
	# python3 /home/zhou/.vim/plugged/YouCompleteMe/install.py

}

init_clangformat() {
	stow -d $conf_dir -t $HOME -R clang-format
}

init_dotfiles() {
	init_packages
	init_symlinks

	case $1 in
		init_sh)
			init_sh
			;;

		init_conf)
			init_conf
			;;


		init_zsh_custom_post)
			init_zsh
			;;


		system_specified)
			system_specified
			;;

		vim)
			init_vim
			;;

		clang_format)
			init_clangformat
			;;

		*)
			system_specified
			init_git
			init_symlinks
			init_sh
			init_conf
			init_zsh
			init_vim
			init_clangformat
			;;
	esac


}


init_dotfiles $1
