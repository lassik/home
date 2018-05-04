if [ "$(uname)" = Darwin ] && which brew >/dev/null 2>&1; then
	if [ -n "$BASH_VERSION" ] && [ -f /usr/local/etc/bash_completion ]; then
		. /usr/local/etc/bash_completion
	fi
	# Homebrew shall override OS X native utilities
	export PATH="/usr/local/bin:$PATH"
fi
