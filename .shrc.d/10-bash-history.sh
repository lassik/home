if [ -n "$BASH_VERSION" ]; then
	export HISTCONTROL=ignoredups
	# Don't preserve shell command history across sessions.
	unset HISTFILE
fi
