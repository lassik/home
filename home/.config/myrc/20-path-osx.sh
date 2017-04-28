if [ "$(uname)" = Darwin ]; then

    # Homebrew shall override OS X native utilities
    export PATH=/usr/local/bin:"$PATH"

    # MacTeX BasicTeX distribution on OSX
    export PATH="$PATH":/usr/local/texlive/2014basic/bin/x86_64-darwin

fi
