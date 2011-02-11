# a Bash shell fragment, for "anything likely to be useful but
# unobtrusive for Anacode team members"
#
# to be sourced from ~/.bashrc
#   
#   export TEAM_TOOLS=~/gitwk-anacode/team_tools.stable
#   source $TEAM_TOOLS/env/bashrc.frag


# Complications,
#  - mac vs. deskpro
#  - internal wired network vs. guest wifi or think-from-home
#  - try not to hit NFS too often, during login + per command


# add this proj to PATH
if [ -d "$TEAM_TOOLS/bin.$USER" ]; then
    export PATH="$TEAM_TOOLS/bin.$USER:$PATH"
fi

if [ -d "$TEAM_TOOLS/bin" ]; then
    export PATH="$TEAM_TOOLS/bin:$PATH"
fi


# Misc stuff
export http_proxy="http://wwwcache.sanger.ac.uk:3128/"
export PRINTER="n318pc"

alias perldoc-5.8.8=/software/perl-5.8.8/bin/perldoc 
