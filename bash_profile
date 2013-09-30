# -*- Shell-script -*-

# anacode environment
anacode_dir=/software/anacode

if [ -d "$anacode_dir" ]
then

# distro-specific directories, appended first so that their contents
# will override the contents of distro-independent directories

# we must use the full path to anacode_distro_code as $anacode_dir/bin
# is not yet on $PATH

anacode_distro_code="$( $anacode_dir/bin/anacode_distro_code )"
if [ -n "$anacode_distro_code" ]
then
    anacode_distro_dir="$anacode_dir/distro/$anacode_distro_code"
    if [ -d "$anacode_distro_dir" ]
    then

        PATH="\
$PATH\
:$anacode_distro_dir/bin\
"

        PERL5LIB="\
$PERL5LIB\
:$anacode_distro_dir/lib\
:$anacode_distro_dir/lib/site_perl\
"

    fi
fi

# choose the Perl
if ! ( echo "$PATH" | sed -e 's/:/\n/g' | grep -E ^/software/perl ); then
    case "$anacode_distro_code" in
        lenny|lucid|squeeze) PATH="/software/perl-5.12.2/bin:$PATH" ;;
        precise) PATH="/software/perl-5.14.4/bin:$PATH" ;;
        etch) # not expecting to need this; 5.12.2 needs GLIBC_2.4
            PATH="/software/perl-5.10.1/bin:$PATH"
            ;;
        *) echo $0: Not sure which Perl we want for distro=$anacode_distro_code, take default $( which perl ) >&2 ;;
    esac
fi

# distro-independent directories

echo $PATH | sed -e 's/:/\n/g' | grep -qE "^$anacode_dir/bin"'/?$' || \
    PATH="\
$PATH\
:$anacode_dir/bin\
"
# May be set in /software/anacode/etc/profile.anacode , see ./swac-etc/

PERL5LIB="\
$PERL5LIB\
:$anacode_dir/lib\
:$anacode_dir/lib/site_perl\
"

fi # if [ -d "$anacode_dir" ]

# team_tools environment

if true &&
    [ -n "$ANACODE_TEAM_TOOLS" ] &&
    [ -d "$ANACODE_TEAM_TOOLS" ]
then

    PATH="\
$PATH\
:$ANACODE_TEAM_TOOLS/bin\
:$ANACODE_TEAM_TOOLS/otterlace/bin\
:$ANACODE_TEAM_TOOLS/otterlace/release/scripts\
"

    PERL5LIB="\
$PERL5LIB\
:$ANACODE_TEAM_TOOLS/perl/lib\
"

fi

# zircon environment
if [ -n "$ANACODE_ZIRCON" ] &&
    [ -f "$ANACODE_ZIRCON/profile.sh" ]
then
    . "$ANACODE_ZIRCON/profile.sh"
fi

PERL5LIB="${PERL5LIB#:}" # in case $PERL5LIB was originally empty

export PATH PERL5LIB

export no_proxy=localhost
export http_proxy=http://webcache.sanger.ac.uk:3128

CVS_RSH=ssh
export CVS_RSH

ANACODE_PERLCRITICRC="$ANACODE_TEAM_TOOLS/perl/perlcriticrc"
export ANACODE_PERLCRITICRC

umask 022

unset \
    anacode_dir \
    anacode_distro_code \

