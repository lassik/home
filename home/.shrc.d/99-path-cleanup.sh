# Remove blank and duplicate entries from PATH.
export PATH="$(echo -n "$PATH" | awk -v RS=: '$0 && !arr[$0]++' | paste -sd : -)"
