if [ -n "$BASH_VERSION" ]; then
    if [ -e ~/etc/profile.d/bash_completion.sh ]; then
        . ~/etc/profile.d/bash_completion.sh
        complete -F _cd -o nospace c
    fi

    if which brew >/dev/null 2>&1 && [ -f $(brew --prefix)/etc/bash_completion ]; then
        . $(brew --prefix)/etc/bash_completion
    fi
fi
