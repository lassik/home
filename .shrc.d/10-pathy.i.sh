if which pathy >/dev/null 2>&1 && [ -n "$BASH_VERSION" ]; then
	eval "$($(which pathy) activate)"
fi
