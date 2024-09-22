for x in /bin \
	/sbin \
	/usr/pkg/bin \
	/usr/pkg/sbin \
	/usr/bin \
	/usr/sbin \
	/usr/X11R6/bin \
	/usr/X11/bin \
	/usr/games; do
	if [ -d "$x" ]; then
		export PATH="$PATH":"$x"
	fi
done
if [ "$(uname)" = Darwin ] && which brew >/dev/null 2>&1; then
	# Homebrew shall override OS X native utilities
	for x in /usr/local/bin /usr/local/sbin; do
		if [ -d "$x" ]; then
			export PATH="$x":"$PATH"
		fi
	done
fi
for x in ~/".local/$(uname | tr A-Z a-z)-$(uname -m | tr A-Z a-z)/bin" \
	~/.local/bin; do
	if [ -d "$x" ]; then
		export PATH="$x":"$PATH"
	fi
done
