# Don't preserve shell command history across sessions.
unset HISTFILE

# Clear all readline command history files for shells, interpreters, etc.
find ~ -maxdepth 1 -type f -name '.*_history' -delete
find ~ -maxdepth 1 -type f -name 'nohup.out' -delete
