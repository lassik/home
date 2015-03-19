# These aliases are from the oh-my-zsh git plugin
alias ga='git add'
alias gap='git add --patch'
alias gb='git branch'
alias gba='git branch -a'
alias gc!='git commit -v --amend'
alias gc='git commit -v'
alias gcm='git checkout master'
alias gco='git checkout'
alias gd='git diff'
alias gdc='git diff --cached'
alias gr='git remote'
alias grv='git remote -v'
alias gst='git status'
alias gsta='git stash'
alias gstd='git stash drop'
alias gstp='git stash pop'
alias gsts='git stash show --text'

if test -r /etc/bash_completion.d/git-completion.bash ; then
    source /etc/bash_completion.d/git-completion.bash
    __git_complete ga    _git_add
    __git_complete gap   _git_add
    __git_complete gb    _git_branch
    __git_complete gba   _git_branch
    __git_complete gc    _git_commit
    __git_complete gc!   _git_commit
    __git_complete gcm   _git_checkout
    __git_complete gco   _git_checkout
    __git_complete gd    _git_diff
    __git_complete gdc   _git_diff
    __git_complete gr    _git_remote
    __git_complete grv   _git_remote
    # no gst
    __git_complete gsta  _git_stash
    __git_complete gstd  _git_stash
    __git_complete gstp  _git_stash
    __git_complete gsts  _git_stash
fi
