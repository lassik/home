for d in ~/bin ~/oracle/bin /usr/local/sbin /bin /sbin /usr/bin /usr/sbin /usr/X11R6/bin /usr/X11/bin /usr/games ; do
    if test -d "$d"; then
        export PATH="$PATH":"$d"
    fi
done
