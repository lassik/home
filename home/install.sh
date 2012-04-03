#! /bin/sh
cd "$(dirname "$0")"
cp="cp" # Change this to "echo cp" for testing.
for src in dot.* ; do
    dst=~/"$(echo "$src" | sed "s/^dot//")"
    if ! test -e "$dst".orig && test -s "$dst" && awk "/lassi/ { exit 1 }" < "$dst" ; then
        # dst.orig doesn't exist; dst exists and doesn't contain my name on the first line
        $cp -pf "$dst" "$dst".orig
    fi
    $cp -pf "$src" "$dst"
done
