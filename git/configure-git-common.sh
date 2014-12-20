# This script is meant to be sourced from the other scripts.
git config --global --unset push.default || :
if git --version | grep 'git version 2' >/dev/null ; then
    git config --global push.default simple
fi
git config --global user.name "Lassi Kortela"
git config --global --unset user.email || :
