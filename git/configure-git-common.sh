# This script is meant to be sourced from the other scripts.

# Change to push.default to 'simple' once I no longer use git <1.7.11
# anywhere. Sigh.
git config --global push.default current

git config --global user.name "Lassi Kortela"
git config --global --unset user.email || :
