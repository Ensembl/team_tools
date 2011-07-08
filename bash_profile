# -*- Shell-script -*-

if true &&
    [ -n "$ANACODE_TEAM_TOOLS" ] &&
    [ -d "$ANACODE_TEAM_TOOLS" ]
then

    export PATH="\
$PATH\
:$ANACODE_TEAM_TOOLS/bin\
:$ANACODE_TEAM_TOOLS/otterlace/server/bin\
"

    export PERL5LIB="\
$ANACODE_TEAM_TOOLS/perl/lib\
"

export no_proxy=localhost
export http_proxy=http://webcache.sanger.ac.uk:3128

fi
