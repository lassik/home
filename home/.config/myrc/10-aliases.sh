# I don't like having my own shell aliases and functions but these are
# almost mandatory.

if which dircolors >/dev/null 2>&1 ; then
    l() {
        ls --color -halF "$@"
    }
else
    l() {
        ls -halF "$@"
    }
fi

c() {
    cd "${1-..}"
}
