if [ "$(uname)" = Darwin ] && which brew >/dev/null 2>&1; then
    :
	# Homebrew shall override OS X native utilities
	#export PATH="/usr/local/bin:$PATH"
fi
