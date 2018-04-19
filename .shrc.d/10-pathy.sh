case "$-" in
*i*)
	# This is an interactive shell.
	if which pathy >/dev/null 2>&1 && [ -n "$BASH_VERSION" ]; then
		eval "$(pathy activate)"
	fi
	;;
esac
