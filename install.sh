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

# install speicified packages
install_package_one() {
  command_line_name="$1"
  ubuntu_package_name="$2"
  darwin_package_name="$3"

  if command -v "$command_line_name" &>/dev/null; then
    return
  fi

  case $PLATFORM in
  linux)
    if [[ "$NAME" == "Ubuntu" ]]; then
      sudo apt-get update && sudo apt-get install -y "$ubuntu_package_name"
    fi
    ;;
  macos) brew install "$darwin_package_name" ;;
  esac
}

stow_dirs() {
  source="$1"
  target="$2"

  shift 2
  packages=("$@")

  if [[ ! -d "$target" ]]; then
    mkdir -p "$target"
  fi

  install_package_one stow stow stow
  stow -d "$source" -t "$target" -R "${packages[@]}"
}

install_node() {
  # Check if nvm is already installed
  if command -v nvm &>/dev/null; then
    echo "nvm is already installed."
    return
  else
    # Install nvm (Node Version Manager)
    echo "Installing nvm..."
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash

    # Source the NVM script to make it available in the current session
    export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

    echo "nvm installation complete."
  fi

  # Verify nvm is now available
  if command -v nvm &>/dev/null; then
    echo "nvm is available."

    # Check if npm is installed
    if command -v npm &>/dev/null; then
      echo "npm is already installed."
    else
      echo "npm is not installed. Installing Node.js (LTS)..."

      # List available LTS versions and install the latest one
      nvm ls-remote --lts
      nvm install --lts

      echo "npm and Node.js (LTS) installation complete."
    fi
  else
    echo "Error: nvm could not be installed. Please check your setup."
    return 1
  fi
}

install_nvim() {
  if command -v nvim &>/dev/null; then
    return
  fi

  if [[ ! -d "neovim" ]]; then
    git clone --depth 1 https://github.com/neovim/neovim.git
  fi
  cd neovim && make CMAKE_BUILD_TYPE=Release && sudo make install
  cd .. && rm -rf neovim
}

install_spf() {
  sudo bash -c "$(curl -sLo- https://superfile.netlify.app/install.sh)"
}

install_fzf() {
  # install fzf
  ~/.fzf/install --all
}

# Install macOS-specific packages
darwin_packages() {
  brew install symlinks stow trash keepassxc luarocks lazygit font-symbols-only-nerd-font font-awesome-terminal-fonts

  pip install --break-system-packages pynvim
}

# Install Linux-specific packages
linux_packages() {
  if [[ $NAME == "Ubuntu" ]]; then
    sudo apt-get update && sudo apt-get install -y \
      build-essential clang-format cmake cscope curl \
      exuberant-ctags git global gnutls-bin golang \
      keepassxc mono-complete python3-dev ripgrep \
      stow symlinks tmux xclip xsel zsh luarocks alacritty shellcheck

    sudo apt purge -y gnome-terminal

    # Install lazygit_
    LAZYGIT_VERSION=$(curl -s "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" | \grep -Po '"tag_name": *"v\K[^"]*')
    curl -Lo lazygit.tar.gz "https://github.com/jesseduffield/lazygit/releases/download/v${LAZYGIT_VERSION}/lazygit_${LAZYGIT_VERSION}_Linux_x86_64.tar.gz"
    tar xf lazygit.tar.gz lazygit
    sudo install lazygit -D -t /usr/local/bin/
    rm -rf lazygit.tar.gz lazygit
  fi
}

# Platform-specific setup
install_packages() {
  case $PLATFORM in
  linux) linux_packages ;;
  macos) darwin_packages ;;
  esac

  install_node
  install_nvim
  install_spf
  install_fzf
}

# Fix broken symlinks
cleanup_symlinks() {
  install_package_one symlinks symlinks symlinks
  sudo symlinks -d /usr/local/bin/ "$HOME"
}

# Initialize shell scripts
install_scripts() {
  stow_dirs "$dotfiles_dir" "/usr/local/bin/" sh
}

# Initialize configuration files
init_conf() {
  mkdir -p "$HOME/.config/clash"

  stow_dirs "$conf_dir" "$HOME" emacs aria2 tmux
}

# Initialize Zsh
init_zsh() {
  stow_dirs "$conf_dir" "$HOME" zsh


  install_package_one zsh zsh zsh
  ZSH_CUSTOM=$zsh_dir/.oh-my-zsh/custom
  mkdir -p "$ZSH_CUSTOM"

  # Clone plugins if not already present
  for plugin in zsh-syntax-highlighting zsh-autosuggestions; do
    plugin_dir="$ZSH_CUSTOM/$plugin"
    if [[ ! -d "$plugin_dir" ]]; then
      git clone "https://github.com/zsh-users/$plugin.git" "$plugin_dir"
    fi
  done
}

# Initialize Vim configuration
init_vim() {
  install_nvim
  stow_dirs "$conf_dir" "$HOME/.config/" .config
}

# Initialize systemd services
init_systemd() {
  stow_dirs "$dotfiles_dir" "$HOME/.config/systemd" systemd

  systemctl --user daemon-reload
  for service in "$systemd_dir"/*; do
    systemctl --user enable --now "$(basename "$service")"
  done
}

# Initialize Git submodules
init_git() {
  install_package_one git git git
  git submodule update --init --recursive --force --remote
}

# Initialize clang-format configuration
init_clangformat() {
  install_package_one stow stow stow
  stow -d "$conf_dir" -t "$HOME" -R clang-format
}

# Main dotfiles initialization
init_dotfiles() {

  case $1 in
  packages) install_packages ;;
  cleanup) cleanup_symlinks ;;
  sh) install_scripts ;;
  conf) init_conf ;;
  zsh) init_zsh ;;
  vim) init_vim ;;
  clang_format) init_clangformat ;;
  *) # Default setup
    install_packages
    cleanup_symlinks
    init_git
    install_scripts
    init_conf
    init_zsh
    init_vim
    ;;
  esac
}

init_dotfiles "${1:-}"
