# Oracle Instant Client stuff... sigh
if test -d ~/oracle ; then
    export ORACLE_HOME=~/oracle
    export TNS_ADMIN=~/oracle
    export DYLD_LIBRARY_PATH=~/oracle/lib
    export PATH="$PATH":~/oracle/bin
fi
