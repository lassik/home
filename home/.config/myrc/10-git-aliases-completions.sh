# These aliases are from the oh-my-zsh git plugin
# https://github.com/robbyrussell/oh-my-zsh/commits/master/plugins/git/git.plugin.zsh

alias ga='git add'
alias gapa='git add --patch'
alias gb='git branch'
alias gba='git branch -a'
alias gc!='git commit -v --amend'
alias gc='git commit -v'
alias gcm='git checkout master'
alias gco='git checkout'
alias gcp='git cherry-pick'
alias gd='git diff'
alias gdca='git diff --cached'
alias gdt='git diff-tree --no-commit-id --name-only -r'
alias gl='git pull'
alias glg='git log --stat --max-count=10'
alias glgg='git log --graph --max-count=10'
alias gp='git push --recurse-submodules=check' # The options are my own addition
alias gr='git remote'
alias grb='git rebase'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias grbi='git rebase -i'
alias grbm='git rebase master'
alias grbs='git rebase --skip'
alias grh='git reset HEAD'
alias grt='cd $(git rev-parse --show-toplevel || echo ".")'
alias grv='git remote -v'
alias gsb='git status -sb'
alias gss='git status -s'
alias gst='git status'
alias gsta='git stash'
alias gstd='git stash drop'
alias gstp='git stash pop'
alias gsts='git stash show --text'
alias gsu='git submodule update'
alias gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
alias gup='git pull --rebase'
alias gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit -m "--wip--"'

if test -n "$BASH_VERSION" && test -r /etc/bash_completion.d/git-completion.bash ; then
    source /etc/bash_completion.d/git-completion.bash

    # no grt
    # no gsb
    # no gss
    # no gst
    # no gunwip
    # no gwip
    __git_complete ga    _git_add
    __git_complete gapa  _git_add
    __git_complete gb    _git_branch
    __git_complete gba   _git_branch
    __git_complete gc    _git_commit
    __git_complete gc!   _git_commit
    __git_complete gcm   _git_checkout
    __git_complete gco   _git_checkout
    __git_complete gcp   _git_cherry_pick
    __git_complete gd    _git_diff
    __git_complete gdca  _git_diff
    __git_complete gdt   _git_diff  # OK?
    __git_complete gl    _git_pull
    __git_complete glg   _git_log
    __git_complete glgg  _git_log
    __git_complete gp    _git_push
    __git_complete gr    _git_remote
    __git_complete grb   _git_rebase
    __git_complete grba  _git_rebase
    __git_complete grbc  _git_rebase
    __git_complete grbi  _git_rebase
    __git_complete grbm  _git_rebase
    __git_complete grbs  _git_rebase
    __git_complete grh   _git_reset
    __git_complete grv   _git_remote
    __git_complete gsta  _git_stash
    __git_complete gstd  _git_stash
    __git_complete gstp  _git_stash
    __git_complete gsts  _git_stash
    __git_complete gsu   _git_submodule
    __git_complete gup   _git_fetch

fi
