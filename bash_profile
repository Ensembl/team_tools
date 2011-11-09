# -*- Shell-script -*-

if true &&
    [ -n "$ANACODE_TEAM_TOOLS" ] &&
    [ -d "$ANACODE_TEAM_TOOLS" ]
then

    export PATH="\
$PATH\
:$ANACODE_TEAM_TOOLS/bin\
:$ANACODE_TEAM_TOOLS/otterlace/server/bin\
:$ANACODE_TEAM_TOOLS/otterlace/release/scripts\
"

    PERL5LIB="\
$PERL5LIB\
:$ANACODE_TEAM_TOOLS/perl/lib\
"
PERL5LIB="${PERL5LIB#:}" # in case $PERL5LIB was originally empty
export PERL5LIB

export no_proxy=localhost
export http_proxy=http://webcache.sanger.ac.uk:3128

fi

CVS_RSH=ssh
export CVS_RSH
