case "$-" in
*i*)
	# This is an interactive shell.
	if which shellprompt >/dev/null 2>&1; then
		if [ -n "$ZSH_VERSION" ]; then
			eval "$(shellprompt activate zsh)"
		elif [ -n "$BASH_VERSION" ]; then
			eval "$(shellprompt activate bash)"
		fi
	else
		export PS1='\n\w\n\u@\h\$ '
	fi
	;;
esac
