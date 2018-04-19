case "$-" in
*i*)
	# This is an interactive shell.
	if which pathy >/dev/null 2>&1; then
		eval "$(pathy activate)"
	fi
	;;
esac
