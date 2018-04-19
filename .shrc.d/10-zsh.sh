if [ -n "$ZSH_VERSION" ]; then
	# Leave background jobs running unless I explicitly kill them.
	setopt NO_HUP
	setopt NO_CHECK_JOBS
fi
