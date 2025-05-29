#!/bin/bash

set -euo pipefail

# Define directories
dotfiles_dir="$(dirname "$(realpath "$0")")"
conf_dir="$dotfiles_dir/conf"
zsh_dir="$conf_dir/zsh"
systemd_dir="$dotfiles_dir/systemd"

# Color codes for logging
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
  printf "${GREEN}[INFO]${NC} %s\n" "$1"
}

log_warn() {
  printf "${YELLOW}[WARN]${NC} %s\n" "$1"
}

log_error() {
  printf "${RED}[ERROR]${NC} %s\n" "$1" >&2
  exit 1
}

# Detect platform
PLATFORM=$(sh sh/systype.sh)

# Load OS release information for Linux
if [[ $PLATFORM == "linux" ]]; then
    source /etc/os-release
fi

# Stow directories helper
stow_dirs() {
  local source="$1"
  local target="$2"
  shift 2
  local packages=("$@")

  [[ ! -d "$target" ]] && mkdir -p "$target"

  if ! command -v stow &>/dev/null; then
    install_package stow
  fi

  sudo stow -d "$source" -t "$target" -R "${packages[@]}"
  log_info "Stowed packages: ${packages[*]}"
}

# Package installation handler
install_package() {
  local pkg="$1"
  local ubuntu_pkg="${2:-$pkg}"
  local darwin_pkg="${3:-$pkg}"

  if command -v "$pkg" &>/dev/null; then
    log_info "$pkg is already installed"
    return 0
  fi

  case $PLATFORM in
  linux)
    [[ "$NAME" != "Ubuntu" ]] && return
    sudo apt-get update && sudo apt-get install -y "$ubuntu_pkg"
    ;;
  macos)
    # Check if Homebrew is installed
    if ! command -v brew &>/dev/null; then
      log_error "Homebrew not found. Please install it first: https://brew.sh"
    fi
    brew install "$darwin_pkg"
    ;;
  *)
    log_warn "Unsupported platform for $pkg installation"
    return 1
    ;;
  esac

  if command -v "$pkg" &>/dev/null; then
    log_info "Successfully installed $pkg"
  else
    log_error "Failed to install $pkg"
  fi
}

# Node.js installation
install_node() {
  if command -v node &>/dev/null; then
    log_info "Node.js is already installed"
    return
  fi

  # Install nvm
  export NVM_DIR="${NVM_DIR:-$HOME/.nvm}"
  if [[ ! -d "$NVM_DIR" ]]; then
    log_info "Installing nvm..."
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  fi

  # Load nvm if not loaded
  if ! command -v nvm &>/dev/null; then
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  fi

  # Install Node.js LTS
  if ! command -v node &>/dev/null; then
    log_info "Installing Node.js LTS..."
    nvm install --lts
  fi

  # Verify installation
  if ! command -v node &>/dev/null; then
    log_error "Node.js installation failed"
  fi
}

# Neovim installation (macOS compatible)
install_nvim() {
  if command -v nvim &>/dev/null; then
    log_info "Neovim is already installed"
    return
  fi

  case $PLATFORM in
  macos)
    brew install neovim
    ;;
  linux)
    local build_dir="/tmp/neovim_build"
    mkdir -p "$build_dir" && cd "$build_dir"

    log_info "Building Neovim from source..."
    git clone --depth 1 https://github.com/neovim/neovim.git
    cd neovim && make CMAKE_BUILD_TYPE=Release && sudo make install

    cd "$dotfiles_dir"
    rm -rf "$build_dir"
    ;;
  esac
}

# Superfile installation
install_spf() {
  if command -v spf &>/dev/null; then
    log_info "Superfile is already installed"
    return
  fi

  log_info "Installing Superfile..."
  sudo bash -c "$(curl -sLo- https://superfile.netlify.app/install.sh)"
}

# fzf installation with platform-specific methods
install_fzf() {
  if command -v fzf &>/dev/null; then
    log_info "fzf is already installed"
    return
  fi

  case $PLATFORM in
  macos)
    brew install fzf
    ;;
  linux)
    if [[ "$NAME" == "Ubuntu" ]]; then
      sudo apt-get install -y fzf
    else
      # Fallback to manual installation
      log_info "Installing fzf manually..."
      local fzf_dir="/tmp/fzf_install"
      git clone --depth 1 https://github.com/junegunn/fzf.git "$fzf_dir"
      "$fzf_dir/install" --all
      rm -rf "$fzf_dir"
    fi
    ;;
  *)
    log_warn "fzf installation not supported on this platform"
    return
    ;;
  esac

  # Run fzf install script for shell integration
  if command -v fzf &>/dev/null; then
    log_info "Configuring fzf shell integration..."
    "$(command -v fzf)" --install --zsh
  fi
}

# Install tabby.sh
install_tabby() {
  if command -v tabby &>/dev/null; then
    log_info "Tabby is already installed"
    return
  elif [[ $PLATFORM == "linux" ]]; then
    log_warn "Tabby installation is only supported on Linux "
    return
  fi

  log_info "Installing Tabby..."
  curl -s https://packagecloud.io/install/repositories/eugeny/tabby/script.deb.sh | sudo bash && sudo apt install -y tabby-terminal
  install_package tabby tabby-terminal tabby
  return
}

# macOS package installation
install_darwin_packages() {
  # Check if Homebrew is installed
  if ! command -v brew &>/dev/null; then
    log_error "Homebrew not found. Please install it first: https://brew.sh"
  fi

  brew install symlinks stow trash keepassxc luarocks lazygit \
    font-symbols-only-nerd-font font-awesome-terminal-fonts tabby

  # Install pynvim for Neovim Python support
  python3 -m pip install pynvim --break-system-packages
}

# Linux package installation
install_linux_packages() {
  source /etc/os-release || true

  [[ "$NAME" != "Ubuntu" ]] && return

  sudo apt-get update && sudo apt-get install -y \
    build-essential clang-format cmake cscope curl \
    exuberant-ctags git global gnutls-bin golang \
    keepassxc mono-complete python3-dev ripgrep \
    stow symlinks tmux xclip xsel zsh luarocks \
    shellcheck

  sudo apt purge -y gnome-terminal

  # Install lazygit
  if ! command -v lazygit &>/dev/null; then
    log_info "Installing lazygit..."
    LAZYGIT_VERSION=$(curl -s "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" | grep -Po '"tag_name": *"v\K[^"]*')
    curl -Lo lazygit.tar.gz "https://github.com/jesseduffield/lazygit/releases/download/v${LAZYGIT_VERSION}/lazygit_${LAZYGIT_VERSION}_Linux_x86_64.tar.gz"
    tar xf lazygit.tar.gz lazygit
    sudo install lazygit /usr/local/bin/
    rm lazygit.tar.gz lazygit
  fi
}

# Main package installation
install_packages() {
  case $PLATFORM in
  linux) install_linux_packages ;;
  macos) install_darwin_packages ;;
  *) log_warn "Unsupported platform: $PLATFORM" ;;
  esac

  install_node
  install_nvim
  install_spf
  install_fzf
  install_tabby
}

# Cleanup symlinks
cleanup_symlinks() {
  install_package symlinks
  sudo symlinks -d /usr/local/bin/ "$HOME"
  log_info "Broken symlinks cleaned"
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

# Initialize Zsh (macOS compatible)
init_zsh() {
  install_package zsh
  stow_dirs "$conf_dir" "$HOME" zsh

  ZSH_CUSTOM="$zsh_dir/.oh-my-zsh/custom/plugins"
  mkdir -p "$ZSH_CUSTOM"

  # Define plugins and their repositories (compatible with older Bash)
  plugins=(
    "zsh-syntax-highlighting:https://github.com/zsh-users/zsh-syntax-highlighting"
    "zsh-autosuggestions:https://github.com/zsh-users/zsh-autosuggestions"
  )

  # Iterate over the plugins
  for plugin_spec in "${plugins[@]}"; do
    plugin="${plugin_spec%%:*}"
    url="${plugin_spec#*:}"
    plugin_dir="$ZSH_CUSTOM/$plugin"

    if [[ ! -d "$plugin_dir" ]]; then
      log_info "Cloning $plugin plugin..."
      git clone "$url" "$plugin_dir"
    fi
  done
}

# Initialize Neovim configuration
init_vim() {
  install_nvim
  stow_dirs "$conf_dir/." "$HOME/.config/" .config
}

# Initialize systemd services
init_systemd() {
  [[ $PLATFORM != "linux" ]] && return

  stow_dirs "$dotfiles_dir" "$HOME/.config/systemd" systemd
  systemctl --user daemon-reload

  for service in "$systemd_dir"/*.service; do
    service_name=$(basename "$service")
    systemctl --user enable --now "$service_name"
    log_info "Enabled service: $service_name"
  done
}

# Initialize Git submodules
init_git() {
  install_package git
  git submodule update --init --recursive --force --remote
  log_info "Git submodules initialized"
}

# Main dotfiles initialization
init_dotfiles() {
  case "${1:-}" in
  packages) install_packages ;;
  cleanup) cleanup_symlinks ;;
  sh) install_scripts ;;
  conf) init_conf ;;
  zsh) init_zsh ;;
  vim) init_vim ;;
  # systemd) init_systemd ;;
  git) init_git ;;
  spf) install_spf ;;
  fzf) install_fzf ;;
  tabby) install_tabby ;;
  *)
    install_packages
    cleanup_symlinks
    init_git
    install_scripts
    init_conf
    init_zsh
    init_vim
    # [[ $PLATFORM == "linux" ]] && init_systemd
    ;;
  esac
}

init_dotfiles "${1:-}"
