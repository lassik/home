case "$-" in
    *i*)
        # This is an interactive shell.
        if which shellprompt >/dev/null 2>&1; then
            if test -n "$ZSH_VERSION"; then
                eval $(shellprompt activate zsh)
            elif test -n "$BASH_VERSION"; then
                eval $(shellprompt activate bash)
            fi
        fi
        ;;
esac
