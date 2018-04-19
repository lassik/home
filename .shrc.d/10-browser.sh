if [ -n "$DISPLAY" ]; then
	for x in chromium-browser; do
		if which "$x" >/dev/null 2>&1; then
			export BROWSER="$x"
			break
		fi
	done
fi
