# I don't like having my own shell aliases and functions but these are
# almost mandatory.

unalias l c >/dev/null 2>&1

if which dircolors >/dev/null 2>&1; then
	l() {
		ls --color -halF "$@"
	}
else
	export CLICOLOR=y
	l() {
		ls -halF "$@"
	}
fi

c() {
	cd "${1-..}"
}
