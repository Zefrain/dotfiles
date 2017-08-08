dir=$(dirname $(readlink ~/.zshrc))

_init() {
    case "$(uname -s)" in
        "Linux")
            source "$dir/.zshrc.linux"
            ;;
        "Darwin")
            source "$dir/.zshrc.osx"

            export PATH="/usr/local/opt/llvm/bin:$PATH"
            ;;
    esac
}

_init
