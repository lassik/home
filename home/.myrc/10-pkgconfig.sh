test -d "$HOME/lib/pkgconfig" && \
    export PKG_CONFIG_PATH=$HOME/lib/pkgconfig:$PKG_CONFIG_PATH
test -d "$HOME/share/pkgconfig" && \
    export PKG_CONFIG_PATH=$HOME/share/pkgconfig:$PKG_CONFIG_PATH
