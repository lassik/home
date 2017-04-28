if [ -n "$BASH_VERSION" ]; then
    if [ -e ~/etc/profile.d/bash_completion.sh ]; then
        . ~/etc/profile.d/bash_completion.sh
        complete -F _cd -o nospace c
    fi
fi
