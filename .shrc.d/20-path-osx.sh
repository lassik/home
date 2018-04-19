if [ "$(uname)" = Darwin ]; then

	# Homebrew shall override OS X native utilities
	export PATH=/usr/local/bin:"$PATH"

fi
