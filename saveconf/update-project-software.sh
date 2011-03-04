#! /bin/sh

set -e
#set -x

# Run with something like
#   cd ~/gitwk--bg/project-software.saveconf && ~/gitwk-anacode/team_tools/saveconf/update-project-software.sh
#
#
# This would be called ... from cron on mca@deskpro17119 ...?


SAVEREPO=$PWD
WANTBASE=project-software.saveconf
if [ -d "$SAVEREPO/.git" ] && [ $( basename $SAVEREPO ) == $WANTBASE ]; then
    :
    # Happy with PWD being our SAVEREPO
    #
    # (It's not just a random clone I have kicking around, but a copy
    # for the purpose of being updated by cron)
else
    echo "$0: Are we in the right directory?  SAVEREPO=$SAVEREPO" >&2
    echo "Expected this to have .../$WANTBASE/.git" >&2
    exit 1
fi

export GIT_DIR=$SAVEREPO/.git


# dotlockfile is in Debian package liblockfile1
trap "dotlockfile -u -p $GIT_DIR.lock" ERR
dotlockfile -l -p $GIT_DIR.lock

# Get up-to-date; make noise only if we fetch something
#git fetch origin
#git merge -q --ff-only origin/master

TIME0=$( date +%s )

# Slurp our relevant files into the index - heavy metadata access!
cd /software/noarch
WARES=$( echo annohelp */{anacode,grit,zfish} )

git add -A $WARES

find $WARES -printf '%M %2n %-8u %-8g %9s %TY-%Tm-%Td %.8TT %p\n' > $SAVEREPO/meta.ls-l 2> $SAVEREPO/meta.err
# does find(1) output in a stably sorted order??

TIME1=$( date +%s )

cd $SAVEREPO
git add -A $SAVEREPO/meta.{ls-l,err}

dotlockfile -c -p $GIT_DIR.lock

git commit -m "updated by $0, add took $[ $TIME1 - $TIME0 ] sec" | \
    (grep -Ev '^# On branch master|^nothing to commit'; true)

git push -q origin

# Update working copy, else we see "Changed but not updated" next time
git reset -q --hard HEAD

dotlockfile -u -p $GIT_DIR.lock
