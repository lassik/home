#! /bin/sh
cp="cp" # Change this to "echo cp" for a dry run.
cd "$(dirname "$0")" # Make sure the current dir is the script dir.
for src in dot.* ; do
    dst=~/"$(echo "$src" | sed "s/^dot//")"
    if ! test -e "$dst".orig && test -s "$dst" && awk "/lassi/ {exit 1} // {exit}" < "$dst" ; then
        # dst.orig doesn't exist; dst exists and isn't empty and
        # doesn't contain my name on the first line
        # so make a backup of dst into dst.orig
        $cp -pf "$dst" "$dst".orig
    fi
    $cp -pf "$src" "$dst"
done
