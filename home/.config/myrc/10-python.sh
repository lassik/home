export PYTHONIOENCODING=UTF-8
if which virtualenvwrapper.sh >/dev/null 2>&1 ; then
    export WORKON_HOME=~/.virtualenvs
    . virtualenvwrapper.sh
fi
