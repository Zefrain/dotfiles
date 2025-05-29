#!/bin/bash

set -euo pipefail

# -------- Logging Helpers --------
log() {
  echo -e "\033[1;32m[INFO]\033[0m $*"
}

warn() {
  echo -e "\033[1;33m[WARN]\033[0m $*" >&2
}

error() {
  echo -e "\033[1;31m[ERROR]\033[0m $*" >&2
}

# -------- Directory Definitions --------
dotfiles_dir="$(dirname "$(realpath "$0")")"
conf_dir="$dotfiles_dir/conf"
zsh_dir="$conf_dir/zsh"
systemd_dir="$dotfiles_dir/systemd"

# -------- Platform Detection --------
PLATFORM=$(sh sh/systype.sh)
[[ $PLATFORM == "linux" ]] && source /etc/os-release || true

# -------- Install a Package --------
install_package_one() {
  local cli_name="$1" ubuntu_pkg="$2" mac_pkg="$3"

  if command -v "$cli_name" &>/dev/null; then
    log "$cli_name is already installed. Skipping."
    return
  fi

  case $PLATFORM in
  linux)
    if [[ "${NAME:-}" == "Ubuntu" ]]; then
      if dpkg -s "$ubuntu_pkg" &>/dev/null; then
        log "$ubuntu_pkg is already installed (dpkg). Skipping."
        return
      fi
      log "Installing $ubuntu_pkg via apt..."
      sudo apt-get update && sudo apt-get install -y "$ubuntu_pkg"
    fi
    ;;
  macos)
    if brew list --formula | grep -q "^$mac_pkg\$"; then
      log "$mac_pkg is already installed (brew). Skipping."
      return
    fi
    log "Installing $mac_pkg via brew..."
    brew install "$mac_pkg"
    ;;
  *)
    warn "Unsupported platform for $cli_name"
    ;;
  esac
}

# -------- Stow Directories --------
stow_dirs() {
  local source="$1" target="$2"
  shift 2
  local packages=("$@")

  mkdir -p "$target"
  install_package_one stow stow stow
  stow -d "$source" -t "$target" -R "${packages[@]}"
}

# -------- Language & Tools Installers --------
install_node() {
  if command -v nvm &>/dev/null; then
    log "nvm already installed."
    return
  fi

  log "Installing nvm..."
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash
  export NVM_DIR="${XDG_CONFIG_HOME:-$HOME/.nvm}"
  [[ -s "$NVM_DIR/nvm.sh" ]] && . "$NVM_DIR/nvm.sh"

  if command -v nvm &>/dev/null; then
    log "nvm is now available."
    if ! command -v npm &>/dev/null; then
      log "Installing latest LTS Node.js..."
      nvm install --lts
    else
      log "npm already installed."
    fi
  else
    error "nvm installation failed."
    return 1
  fi
}

install_nvim() {
  if command -v nvim &>/dev/null; then
    log "Neovim already installed."
    return
  fi

  log "Installing Neovim from source..."
  git clone --depth 1 https://github.com/neovim/neovim.git
  pushd neovim
  make CMAKE_BUILD_TYPE=Release && sudo make install
  popd && rm -rf neovim
}

install_spf() {
  if command -v spf &>/dev/null; then
    log "spf already installed."
    return
  fi

  log "Installing spf..."
  sudo bash -c "$(curl -sLo- https://superfile.netlify.app/install.sh)"
}

install_fzf() {
  log "Installing fzf..."
  ~/.fzf/install --all
}

# -------- Platform Specific Packages --------
install_darwin_packages() {
  log "Installing macOS packages..."
  brew install symlinks stow trash keepassxc luarocks lazygit \
    font-symbols-only-nerd-font font-awesome-terminal-fonts
  pip install --break-system-packages pynvim
}

install_linux_packages() {
  if [[ "${NAME:-}" == "Ubuntu" ]]; then
    log "Installing Linux packages..."
    sudo apt-get update && sudo apt-get install -y \
      build-essential clang-format cmake cscope curl \
      exuberant-ctags git global gnutls-bin golang \
      keepassxc mono-complete python3-dev ripgrep \
      stow symlinks tmux xclip xsel zsh luarocks alacritty shellcheck

    sudo apt purge -y gnome-terminal

    log "Installing lazygit..."
    LAZYGIT_VERSION=$(curl -s https://api.github.com/repos/jesseduffield/lazygit/releases/latest | grep -Po '"tag_name": *"v\K[^"]*')
    curl -Lo lazygit.tar.gz "https://github.com/jesseduffield/lazygit/releases/download/v${LAZYGIT_VERSION}/lazygit_${LAZYGIT_VERSION}_Linux_x86_64.tar.gz"
    tar xf lazygit.tar.gz lazygit
    sudo install lazygit -D -t /usr/local/bin/
    rm -f lazygit.tar.gz lazygit
  fi
}

# -------- Install Meta --------
install_packages() {
  case $PLATFORM in
  linux) install_linux_packages ;;
  macos) install_darwin_packages ;;
  *) warn "Unsupported platform: $PLATFORM" ;;
  esac

  install_node
  install_nvim
  install_spf
  install_fzf
}

# -------- Setup Functions --------
cleanup_symlinks() {
  install_package_one symlinks symlinks symlinks
  log "Cleaning up broken symlinks..."
  sudo symlinks -d /usr/local/bin/ "$HOME"
}

install_scripts() {
  stow_dirs "$dotfiles_dir" "/usr/local/bin/" sh
}

init_conf() {
  mkdir -p "$HOME/.config/clash"
  stow_dirs "$conf_dir" "$HOME" emacs aria2 tmux
}

init_zsh() {
  stow_dirs "$conf_dir" "$HOME" zsh
  install_package_one zsh zsh zsh

  local ZSH_CUSTOM="$zsh_dir/.oh-my-zsh/custom"
  mkdir -p "$ZSH_CUSTOM"

  for plugin in zsh-syntax-highlighting zsh-autosuggestions; do
    plugin_dir="$ZSH_CUSTOM/$plugin"
    [[ ! -d "$plugin_dir" ]] && git clone "https://github.com/zsh-users/$plugin.git" "$plugin_dir"
  done
}

init_vim() {
  install_nvim
  stow_dirs "$conf_dir" "$HOME/.config/" .config
}

init_systemd() {
  stow_dirs "$dotfiles_dir" "$HOME/.config/systemd" systemd
  systemctl --user daemon-reload

  for service in "$systemd_dir"/*; do
    systemctl --user enable --now "$(basename "$service")"
  done
}

init_git() {
  install_package_one git git git
  log "Initializing git submodules..."
  git submodule update --init --recursive --force --remote
}

init_clangformat() {
  install_package_one stow stow stow
  stow -d "$conf_dir" -t "$HOME" -R clang-format
}

# -------- Main Entrypoint --------
init_dotfiles() {
  case "${1:-}" in
  packages) install_packages ;;
  cleanup) cleanup_symlinks ;;
  sh) install_scripts ;;
  conf) init_conf ;;
  zsh) init_zsh ;;
  vim) init_vim ;;
  clang_format) init_clangformat ;;
  *)
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
