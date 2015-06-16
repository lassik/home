export PAGER="less"
export LESS="--ignore-case"
if which src-hilite-lesspipe.sh >/dev/null 2>&1 ; then
    export LESS="$LESS -R"
    export LESSOPEN="| src-hilite-lesspipe.sh %s"
fi
