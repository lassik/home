for e in jmacs mg nano vi ; do
    if which "$e" >/dev/null 2>&1 ; then
        export EDITOR="$e"
        break
    fi
done
export ALTERNATE_EDITOR="$EDITOR"
which emacsclient >/dev/null 2>&1 && export EDITOR=emacsclient
