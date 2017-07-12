source $HOME/Working/zxpay/pts/etc/ptsenv.sh

_init() {
    case "$(uname -s)" in
        "Linux")
            source ~/Git/sync/conf/zsh/.zshrc.linux
            ;;
        "Darwin")
            source ~/Git/sync/conf/zsh/.zshrc.osx
            ;;
    esac
}

_init

export PATH="/usr/local/opt/llvm/bin:$PATH"
