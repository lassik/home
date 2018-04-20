if [ -n "$BASH_VERSION" ] && [ -r /etc/bash_completion.d/git-completion.bash ]; then
	. /etc/bash_completion.d/git-completion.bash

	# no grt
	# no gsb
	# no gss
	# no gst
	# no gunwip
	# no gwip
	__git_complete ga _git_add
	__git_complete gapa _git_add
	__git_complete gb _git_branch
	__git_complete gba _git_branch
	__git_complete gc _git_commit
	__git_complete gc! _git_commit
	__git_complete gcm _git_checkout
	__git_complete gco _git_checkout
	__git_complete gcp _git_cherry_pick
	__git_complete gd _git_diff
	__git_complete gdca _git_diff
	__git_complete gdt _git_diff # OK?
	__git_complete gl _git_pull
	__git_complete glg _git_log
	__git_complete glgg _git_log
	__git_complete gp _git_push
	__git_complete gr _git_remote
	__git_complete grb _git_rebase
	__git_complete grba _git_rebase
	__git_complete grbc _git_rebase
	__git_complete grbi _git_rebase
	__git_complete grbm _git_rebase
	__git_complete grbs _git_rebase
	__git_complete grh _git_reset
	__git_complete grv _git_remote
	__git_complete gsta _git_stash
	__git_complete gstd _git_stash
	__git_complete gstp _git_stash
	__git_complete gsts _git_stash
	__git_complete gsu _git_submodule
	__git_complete gup _git_fetch

fi
