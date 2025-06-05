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
log_info() { printf "${GREEN}[INFO]${NC} %s\n" "$1"; }
log_warn() { printf "${YELLOW}[WARN]${NC} %s\n" "$1"; }
log_error() {
  printf "${RED}[ERROR]${NC} %s\n" "$1" >&2
  exit 1
}

# Detect platform
PLATFORM=$(sh sh/systype.sh)

# Load OS release information for Linux
[[ $PLATFORM == "linux" ]] && source /etc/os-release

# Stow directories helper
stow_dirs() {
  local source="$1"
  local target="$2"
  shift 2
  local packages=("$@")

  [[ ! -d "$target" ]] && mkdir -p "$target"

  install_packages_entry stow
  sudo -A -E stow -d "$source" -t "$target" -R "${packages[@]}"
  log_info "Stowed packages: ${packages[*]}"
}

# Package installation handler
install_packages_entry() {

  local packages=("$@")
  local to_install=()
  [[ ${#packages[@]} -eq 0 ]] && return

  case $PLATFORM in
  linux)
    [[ "$NAME" != "Ubuntu" ]] && return

    for pkg in "${packages[@]}"; do
      if dpkg -s "$pkg" &>/dev/null; then
        log_info "$pkg is already installed"
      else
        to_install+=("$pkg")
      fi
    done

    if [[ ${#to_install[@]} -gt 0 ]]; then
      sudo -A -E apt update
      sudo -A -E apt-get install -y "${to_install[@]}"
    fi
    ;;
  macos)
    for pkg in "${packages[@]}"; do
      if ! brew info --json=v2 "$pkg" 2>/dev/null | jq -e '(.formulae + .casks)[]?.installed | length > 0' >/dev/null; then
        to_install+=("$pkg")
      else
        log_info "$pkg is already installed"
      fi
    done

    if [[ ${#to_install[@]} -gt 0 ]]; then
      brew update
      brew install "${to_install[@]}"
    fi
    ;;
  *)
    log_warn "Unsupported platform for $pkg installation"
    return 1
    ;;
  esac
}

install_node() {
  if command -v node &>/dev/null; then
    log_info "Node.js is already installed"
    return
  fi

  export NVM_DIR="${NVM_DIR:-$HOME/.nvm}"
  if [[ ! -d "$NVM_DIR" ]]; then
    log_info "Installing nvm..."
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.0/install.sh | bash
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
  fi

  if ! command -v nvm &>/dev/null; then
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
  fi

  if ! command -v node &>/dev/null; then
    log_info "Installing Node.js LTS..."
    nvm install --lts
  fi

  if ! command -v node &>/dev/null; then
    log_error "Node.js installation failed"
  fi
}

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
    cd neovim && make CMAKE_BUILD_TYPE=Release && sudo -A -E make install
    cd "$dotfiles_dir"
    rm -rf "$build_dir"
    ;;
  esac
}

install_spf() {
  if command -v spf &>/dev/null; then
    log_info "Superfile is already installed"
    return
  fi

  log_info "Installing Superfile..."
  curl -sLo /tmp/install_spf.sh https://superfile.netlify.app/install.sh
  chmod +x /tmp/install_spf.sh
  sudo -A -E /tmp/install_spf.sh
  rm -f /tmp/install_spf.sh
}

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
      sudo -A -E apt-get install -y fzf
    else
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

  if command -v fzf &>/dev/null; then
    log_info "Configuring fzf shell integration..."
    "$(command -v fzf)" --install --zsh
  fi
}

install_darwin_packages() {
  if ! command -v brew &>/dev/null; then
    log_error "Homebrew not found. Please install it first: https://brew.sh"
  fi

  install_packages_entry symlinks stow trash keepassxc luarocks lazygit \
    font-symbols-only-nerd-font font-awesome-terminal-fonts tabby

  python3 -m pip install pynvim --break-system-packages
}

install_linux_packages() {
  source /etc/os-release || true
  [[ "$NAME" != "Ubuntu" ]] && return

  curl -s https://packagecloud.io/install/repositories/eugeny/tabby/script.deb.sh |
    sudo -A -E bash
  sudo -A -E apt install -y tabby-terminal

  install_packages_entry \
    build-essential clang-format cmake cscope curl \
    exuberant-ctags git global gnutls-bin golang \
    keepassxc mono-complete python3-dev ripgrep \
    stow symlinks tmux xclip xsel zsh luarocks \
    shellcheck tabby-terminal

  sudo -A -E apt purge -y gnome-terminal

  if ! command -v lazygit &>/dev/null; then
    log_info "Installing lazygit..."
    LAZYGIT_VERSION=$(curl -s "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" |
      grep -Po '"tag_name": *"v\K[^"]*')
    curl -Lo lazygit.tar.gz \
      "https://github.com/jesseduffield/lazygit/releases/download/v${LAZYGIT_VERSION}/lazygit_${LAZYGIT_VERSION}_Linux_x86_64.tar.gz"
    tar xf lazygit.tar.gz lazygit
    sudo -A -E install lazygit /usr/local/bin/
    rm lazygit.tar.gz lazygit
  fi
}

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
}

cleanup_symlinks() {
  install_packages_entry symlinks
  sudo -A -E symlinks -d /usr/local/bin/ "$HOME"
  log_info "Broken symlinks cleaned"
}

install_scripts() {
  stow_dirs "$dotfiles_dir" "/usr/local/bin/" sh
}

init_conf() {
  mkdir -p "$HOME/.config/clash"
  stow_dirs "$conf_dir" "$HOME" emacs aria2 tmux
}

init_zsh() {
  install_packages_entry zsh
  stow_dirs "$conf_dir" "$HOME" zsh

  ZSH_CUSTOM="$zsh_dir/.oh-my-zsh/custom/plugins"
  mkdir -p "$ZSH_CUSTOM"

  plugins=(
    "zsh-syntax-highlighting:https://github.com/zsh-users/zsh-syntax-highlighting"
    "zsh-autosuggestions:https://github.com/zsh-users/zsh-autosuggestions"
  )

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

init_vim() {
  install_nvim
  stow_dirs "$conf_dir/." "$HOME/.config/" .config
}

init_git() {
  install_packages_entry git
  git submodule update --init --recursive --force --remote
  log_info "Git submodules initialized"
}

init_dotfiles() {
  case "${1:-}" in
  packages) install_packages ;;
  cleanup) cleanup_symlinks ;;
  sh) install_scripts ;;
  conf) init_conf ;;
  zsh) init_zsh ;;
  vim) init_vim ;;
  git) init_git ;;
  spf) install_spf ;;
  fzf) install_fzf ;;
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

# Prompt once for sudo -A -E password
read -rs -p "Enter your sudo -A -E password (will be used temporarily): " PASSWORD
echo
export SUDO_PASSWORD="$PASSWORD"

# Setup SUDO_ASKPASS script
ASKPASS_SCRIPT="$(mktemp)"
cat >"$ASKPASS_SCRIPT" <<EOF
#!/bin/bash
echo "\$SUDO_PASSWORD"
EOF
chmod +x "$ASKPASS_SCRIPT"
export SUDO_ASKPASS="$ASKPASS_SCRIPT"

# Ensure cleanup
trap 'rm -f "$ASKPASS_SCRIPT"' EXIT
init_dotfiles "${1:-}"
