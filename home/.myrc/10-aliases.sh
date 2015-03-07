# I don't like having my own shell aliases and functions but these are
# almost mandatory.

l() {
    ls -hAlF "$@"
}

c() {
    cd "${1-..}"
}
