if [ "$(uname)" = Darwin ]; then
	export HOMEBREW_CASK_OPTS="--appdir=/Applications"
	# Homebrew shall override OS X native utilities
	export PATH=/usr/local/bin:"$PATH"
fi
