source ~/.shrc
if test -e ~/persist/private/code/shellprompt/shellprompt-activate.zsh ; then
    source ~/persist/private/code/shellprompt/shellprompt-activate.zsh
fi

# Leave background jobs running unless I explicitly kill them.
setopt NO_HUP
setopt NO_CHECK_JOBS
