if test -e ~/etc/profile.d/bash_completion.sh ; then
    source ~/etc/profile.d/bash_completion.sh
    complete -F _cd -o nospace c
fi
