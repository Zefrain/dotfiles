zsh_dir=$(dirname $(readlink ~/.zshrc))

_init() {
    case "$(uname -s)" in
        "Linux")
            source "$zsh_dir/.zshrc.linux"
            ;;
        "Darwin")
            source "$zsh_dir/.zshrc.osx"

      export PATH="/usr/local/opt/llvm/bin:$PATH"
            ;;
    esac

}

_init
