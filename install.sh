#!/bin/bash

set -euxo pipefail

# Define directories
dotfiles_dir="$(dirname "$(realpath "$0")")"
conf_dir="$dotfiles_dir/conf"
zsh_dir="$conf_dir/zsh"
systemd_dir="$dotfiles_dir/systemd"

# Detect platform
PLATFORM=$(sh sh/systype.sh)

# Get OS release information
get_os_release() {
    if [[ $PLATFORM == "linux" ]]; then
        source /etc/os-release
    fi
}

# Initialize shell scripts
init_sh() {
    sudo symlinks -d /usr/local/bin/
    sudo stow -d "$dotfiles_dir" -t /usr/local/bin -R sh
}

# Initialize configuration files
init_conf() {
    mkdir -p "$HOME/.config/clash"
    stow -d "$conf_dir" -t "$HOME" -R emacs aria2 tmux
}

# Initialize Zsh
init_zsh() {
    rm -f "$HOME/.zshrc"
    stow -d "$conf_dir" -t "$HOME" -R zsh
    sed -i -e "s|#\? \?ZSH_CUSTOM=.*|ZSH_CUSTOM=${zsh_dir}/omz_custom|g" "$HOME/.zshrc"

    custom_plugins="$HOME/.oh-my-zsh/custom/plugins"
    [[ -d "$custom_plugins" ]] || return

    for plugin in zsh-syntax-highlighting zsh-autosuggestions; do
        plugin_dir="$custom_plugins/$plugin"
        [[ -d "$plugin_dir" ]] || git clone "https://github.com/zsh-users/$plugin.git" "$plugin_dir"
    done
}

# Initialize systemd services
init_systemd() {
    sudo apt install -y xsel
    mkdir -p "$HOME/.config/systemd"
    stow -d "$dotfiles_dir" -t "$HOME/.config/systemd" systemd
    systemctl --user daemon-reload

    for service in "$systemd_dir"/*; do
        service_name=$(basename "$service")
        systemctl --user enable --now "$service_name"
    done
}

# Install macOS-specific packages
darwin_specified() {
    brew install symlinks stow node ccls trash vim keepassxc
}

# Install Linux-specific packages
linux_specified() {
    git submodule deinit -f workflows/Ariafred
    if [[ $NAME == "Ubuntu" ]]; then
		sudo apt update && sudo apt install -y \
			build-essential \
			ccls \
			clang-format \
			cmake \
			cscope \
			curl \
			default-jdk \
			exuberant-ctags \
			git \
			global \
			gnutls-bin \
			golang \
			keepassxc \
			mono-complete \
			nodejs \
			npm \
			python3-dev \
			ripgrep \
			stow \
			symlinks \
			tmux \
			vim-nox \
			xclip \
		       	xsel \
			zsh
    fi

}

# Platform-specific setup
system_specified() {
    case $PLATFORM in
        linux) linux_specified ;;
        macos) darwin_specified ;;
    esac
}

# Initialize Git submodules
init_git() {
    git submodule update --init --recursive --force --remote
}

# Fix broken symlinks
init_symlinks() {
    symlinks -d "$HOME"
}

# Initialize Vim configuration
init_vim() {
    curl -fsLo "$HOME/.vim/autoload/plug.vim" --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    stow -d "$conf_dir" -t "$HOME" -R vim
    vim +PlugUninstall +qall
    vim +PlugInstall +qall
}

# Initialize clang-format configuration
init_clangformat() {
    stow -d "$conf_dir" -t "$HOME" -R clang-format
}

# Main dotfiles initialization
init_dotfiles() {
    get_os_release
    system_specified
    init_symlinks

    case $1 in
        init_sh) init_sh ;;
        init_conf) init_conf ;;
        init_zsh_custom_post) init_zsh ;;
        vim) init_vim ;;
        clang_format) init_clangformat ;;
        *)  # Default setup
            init_git
            init_symlinks
            init_sh
            init_conf
            init_zsh
            init_vim
            ;;
    esac
}

init_dotfiles "$1"
