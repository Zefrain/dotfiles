#!/bin/bash

set -euxo pipefail

# Define directories
dotfiles_dir="$(dirname "$(realpath "$0")")"
conf_dir="$dotfiles_dir/conf"
zsh_dir="$conf_dir/zsh"
systemd_dir="$dotfiles_dir/systemd"

# Detect platform
PLATFORM=$(sh sh/systype.sh)

# Load OS release information for Linux
[[ $PLATFORM == "linux" ]] && source /etc/os-release || true

# Install macOS-specific packages
darwin_specified() {
    brew install symlinks stow ccls trash vim keepassxc node
}

# Install Linux-specific packages
linux_specified() {
    # installs nvm (Node Version Manager)
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash

    # Source the NVM script to make it available in the current session
    export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

    # download and install Node.js (you may need to restart the terminal)
    nvm install 22

    # verifies the right Node.js version is in the environment
    node -v # should print `v22.11.0`

    # verifies the right npm version is in the environment
    npm -v # should print `10.9.0`

    if [[ $NAME == "Ubuntu" ]]; then
        sudo apt update && sudo apt install -y \
            build-essential ccls clang-format cmake cscope curl \
            exuberant-ctags git global gnutls-bin golang \
            keepassxc mono-complete python3-dev ripgrep \
            stow symlinks tmux vim-nox xclip xsel zsh
    fi
}

# Platform-specific setup
install_system_packages() {
    case $PLATFORM in
        linux) linux_specified ;;
        macos) darwin_specified ;;
    esac
}

# Fix broken symlinks
cleanup_symlinks() {
    symlinks -d "$HOME"
}

# Initialize shell scripts
install_scripts() {
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
    stow -d "$conf_dir" -t "$HOME" -R zsh
    ZSHRC_PATH="$HOME/.zshrc"

    # Resolve the actual file if it's a symlink
    if [[ -L "$ZSHRC_PATH" ]]; then
        ZSHRC_PATH=$(realpath "$ZSHRC_PATH")
    fi

    # Modify ZSHRC if it's a regular file
    if [[ -f "$ZSHRC_PATH" ]]; then
        sed -i -e "s|#\? \?ZSH_CUSTOM=.*|ZSH_CUSTOM=${zsh_dir}/omz_custom|g" "$ZSHRC_PATH"
    else
        echo "Error: $ZSHRC_PATH is not a regular file"
    fi

    custom_plugins="$HOME/.oh-my-zsh/custom/plugins"
    mkdir -p "$custom_plugins"

    # Clone plugins if not already present
    for plugin in zsh-syntax-highlighting zsh-autosuggestions; do
        plugin_dir="$custom_plugins/$plugin"
        if [[ ! -d "$plugin_dir" ]]; then
            git clone "https://github.com/zsh-users/$plugin.git" "$plugin_dir"
        fi
    done
}

# Initialize Vim configuration
init_vim() {
    stow -d "$conf_dir" -t "$HOME" -R vim 

    # Install or update vim-plug
    plug_path="$HOME/.vim/autoload/plug.vim"
    plug_url="https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

    if [[ ! -f "$plug_path" || "$(curl -fsL "$plug_url" | sha256sum)" != "$(sha256sum "$plug_path")" ]]; then
        curl -fsLo "$plug_path" --create-dirs "$plug_url"
    fi

    vim +PlugClean! +PlugInstall +qall
}

# Initialize systemd services
init_systemd() {
    sudo apt install -y xsel
    mkdir -p "$HOME/.config/systemd"
    stow -d "$dotfiles_dir" -t "$HOME/.config/systemd" systemd
    systemctl --user daemon-reload

    for service in "$systemd_dir"/*; do
        systemctl --user enable --now "$(basename "$service")"
    done
}

# Initialize Git submodules
init_git() {
    git submodule update --init --recursive --force --remote
}

# Initialize clang-format configuration
init_clangformat() {
    stow -d "$conf_dir" -t "$HOME" -R clang-format
}

# Main dotfiles initialization
init_dotfiles() {
    install_system_packages
    cleanup_symlinks

    case $1 in
        packages) install_system_packages;;
        cleanup) cleanup_symlinks;;
        sh) install_scripts ;;
        conf) init_conf ;;
        zsh) init_zsh ;;
        vim) init_vim ;;
        clang_format) init_clangformat ;;
        *)  # Default setup
            init_git
            install_scripts
            init_conf
            init_zsh
            init_vim
            ;;
    esac
}

init_dotfiles "${1:-}"

