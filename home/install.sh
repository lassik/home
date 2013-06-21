#! /bin/sh
set -e
cd "$(dirname "$0")"

calcsum() {
    sed 's/checksum [a-z0-9]*/checksum placeholder/' < "$1" | shasum | sed 's/ .*//'
}

putsum () {
    sed "s/checksum [a-z0-9]*/checksum $(calcsum "$1")/" < "$1"
}

getsum() {
    grep 'checksum [a-z0-9]*' < "$1" | head -n 1 | sed 's/.*checksum \([a-z0-9]*\).*/\1/'
}

for src in dot.* ; do
    dst=~/"$(echo "$src" | sed "s/^dot//")"
    update=0
    if test "$(getsum "$src")" != "placeholder" ; then
        echo "skipping $dst (checksum placeholder not found in source file)"
        update=0
    elif ! test -e "$dst" ; then
        echo "creating $dst"
        update=1
    elif test "$(getsum "$dst")" = "" ; then
        echo "skipping $dst (no checksum in destination file)"
        update=0
    elif test "$(getsum "$dst")" = "placeholder" ; then
        echo "skipping $dst (checksum placeholder instead of real checksum in $dst)"
        update=0
    elif test "$(calcsum "$dst")" = "$(calcsum "$src")" ; then
        #echo "unmodified $dst"
        update=0
    elif test "$(getsum "$dst")" = "$(calcsum "$dst")" ; then
        echo "updating $dst"
        update=1
    else
        echo "preserving $dst (appears to be modified outside repo)"
        diff -u "$src" "$dst" || :
        update=0
    fi
    if test "$update" = 1 ; then
        putsum "$src" > "$dst".new
        mv -f "$dst".new "$dst"
    fi
done
