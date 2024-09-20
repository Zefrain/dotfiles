#!/bin/bash

set -e

dotfiles_dir=$(dirname $(realpath $0))

conf_dir="$dotfiles_dir/conf"
zsh_dir="$conf_dir/zsh"
systemd_dir="$dotfiles_dir/systemd"


PLATFORM=$(sh sh/systype.sh)

get_os_release() {
	if [ $PLATFORM == "linux" ] ; then
		OS_RELEASE=$(lsb_release -d | awk '{print $2}')
	fi
}

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
	sudo symlinks -d /usr/local/bin/
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
	sed -i -e "s|#\? \?ZSH_CUSTOM=.*|ZSH_CUSTOM=${zsh_dir}\/omz_custom|g" $(realpath $HOME/.zshrc)

	if [ -e "~/.oh-my-zsh/custom/plugins" ]; then
		if [ ! -e "~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting" ]; then
			git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
		fi

		if [ ! -e "~/.oh-my-zsh/custom/plugins/zsh-autosuggestions" ] ; then
			git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
		fi
	fi

}

init_systemd() {
	stow -d . -t $HOME/.config/systemd systemd
	systemctl --user daemon-reload
	cd $systemd_dir
	for service in $(ls *); do
		systemctl --user start $service
		systemctl --user enable $service
	done
}

darwin_specified() {
	# brew tap mycli
	brew install symlinks stow node ccls trash vim calibre dropbox keepassxc
	echo PATH=/opt/homebrew/bin/:$PATH >> ~/.zshrc
	return
}

do_ubuntu_install() {
	. /etc/os-release

	if [ "${ID}" = "ubuntu" ]; then
		sudo apt update
		sudo apt install -y stow xsel gnutls-bin zsh curl stow symlinks tmux vim symlinks ccls calibre keepassxc nodejs
	fi
}

linux_specified() {
	do_ubuntu_install
	# init_systemd
}

system_specified() {
	if [ "${PLATFORM}" = "linux" ]; then
		git submodule deinit -f workflows/Ariafred

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
	vim +PlugUninstal +q
	vim +PlugInstall +qall 
	# python3 /home/zhou/.vim/plugged/YouCompleteMe/install.py

}

init_clangformat() {
	stow -d $conf_dir -t $HOME -R clang-format
}

init_dotfiles() {

	system_specified
	init_packages
	init_symlinks
	get_os_release

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

		# vim)
		# 	init_vim
		# 	;;

		clang_format)
			init_clangformat
			;;

		*)
			init_git
			init_symlinks
			init_sh
			init_conf
			init_zsh
			init_vim
			;;
	esac

	sudo su - $USER
}

init_dotfiles $1
