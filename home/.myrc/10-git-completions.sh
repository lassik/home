for x in git-completion.bash ; do
    if test -e /etc/bash_completion.d/"$x" ; then
        source /etc/bash_completion.d/"$x"
    fi
done
