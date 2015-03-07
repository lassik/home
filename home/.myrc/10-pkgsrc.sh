# pkgsrc needs to be told where to download packages on operating systems that use it.
if test "$(uname)" = "NetBSD"; then
    export PKG_PATH="ftp://ftp.netbsd.org/pub/pkgsrc/packages/$(uname)/$(uname -m)/$(uname -r)/All/"
fi
