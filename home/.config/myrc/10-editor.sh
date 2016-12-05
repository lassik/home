for e in mg nano; do
    if which "$e" >/dev/null 2>&1 ; then
        export EDITOR="$e"
        export ALTERNATE_EDITOR="$EDITOR"
        break
    fi
done
