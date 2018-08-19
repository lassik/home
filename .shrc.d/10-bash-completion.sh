if [ -n "$BASH_VERSION" ]; then
	if [[ -f ~/etc/profile.d/bash_completion.sh ]]; then
		. ~/etc/profile.d/bash_completion.sh
	fi
	if [[ -f /usr/local/etc/bash_completion ]]; then
		. /usr/local/etc/bash_completion
	fi
	if declare -F _cd >/dev/null; then
		complete -F _cd -o nospace c
	fi
fi
