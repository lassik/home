if which git >/dev/null 2>&1 && ! test -e ~/.gitconfig.lock; then

    # Change to push.default to 'simple' once I no longer use git <1.7.11
    # anywhere. Sigh.
    git config --global push.default current

    # Make less display ANSI colors from git output, not "ESC" junk.
    git config --global core.pager "less -R"

    git config --global user.name "Lassi Kortela"
    git config --global user.email "lassi@lassikortela.net"

fi
