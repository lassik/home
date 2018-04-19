for x in mg nano; do
	if which "$x" >/dev/null 2>&1; then
		export EDITOR="$x"
		export ALTERNATE_EDITOR="$EDITOR"
		break
	fi
done
