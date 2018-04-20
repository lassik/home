if [ "$(uname)" = Darwin ] && which brew >/dev/null 2>&1; then
	x="$(brew --prefix)"
	# Homebrew shall override OS X native utilities
	export PATH="$x/bin:$PATH"
	if [ -n "$BASH_VERSION" ] && [ -f "$x/etc/bash_completion" ]; then
		. "$x/etc/bash_completion"
	fi
fi
