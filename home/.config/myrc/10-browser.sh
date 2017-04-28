if [ -n "$DISPLAY" ]; then
    for b in chromium-browser ; do
        if which "$b" >/dev/null 2>&1 ; then
            export BROWSER="$b"
            break
        fi
    done
fi
