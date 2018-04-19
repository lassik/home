case "$-" in
*i*)
	# This is an interactive shell.
	export PS1='\n\w\n\u@\h\$ '
	case "$(uname)" in
	DragonFly | FreeBSD | NetBSD) export PS1="${PWD}\$ " ;;
	esac
	if which shellprompt >/dev/null 2>&1; then
		if [ -n "$ZSH_VERSION" ]; then
			eval "$(shellprompt activate zsh)"
		elif [ -n "$BASH_VERSION" ]; then
			eval "$(shellprompt activate bash)"
		fi
	fi
	;;
esac
