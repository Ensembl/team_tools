#! /bin/bash

set -e

# Run with something like
#   cd ~/gitwk--bg/server-config-trail.saveconf && ~/gitwk-anacode/team_tools/saveconf/update.sh
#
#
# This is called every 15 minutes from cron on mca@deskpro17119 as of 2011-02-17
#
# Note that the dotlockfile invocation assumes we run from only one
# host, so uses the -p flag


SAVEREPO=$PWD
if [ -d "$SAVEREPO/.git" ] && [ $( basename $SAVEREPO ) = "server-config-trail.saveconf" ]; then
    :
    # Happy with PWD being our SAVEREPO
    #
    # (It's not just a random clone I have kicking around, but a copy
    # for the purpose of being updated by cron)
else
    echo "$0: Are we in the right directory?  SAVEREPO=$SAVEREPO" >&2
    echo "Expected this to have .../server-config-trail/.git" >&2
    exit 1
fi

SAVECONFDIR=$( dirname $0 )
DATADIR=/nfs/WWWdev/SANGER_docs/data/otter

export GIT_DIR=$SAVEREPO/.git
unset http_proxy

# dotlockfile is in Debian package liblockfile1
LOCKFN=$GIT_DIR/saveconf.lock
if dotlockfile -l -p $LOCKFN; then
    trap "dotlockfile -u -p $LOCKFN" ERR
else
    echo dotlockfile failed: exit code $? >&2
    head -v $LOCKFN >&2
    exit 1
fi

# Get up-to-date; make noise only if we fetch something
git fetch origin
git merge -q --ff-only origin/master

mkdir -p $SAVEREPO/meta

TIME0=$( date +%s )

# List dev files
find $DATADIR -printf '%M %2n %-8u %-8g %9s %TY-%Tm-%Td %.8TT %p\n' > $SAVEREPO/meta/ls.-l

# Copy dev files to commit-staging
(
    # Anything in these $DATADIR/??  directories: add, modify, delete
    cd $DATADIR

    # Bodge: don't want to "add -A ." because that would remove
    # top-level files which don't exist here, but we want in the
    # tracking repo anyway.  This will allow _named_ files
    # no-longer-present to disappear from the repo.
    git rm --cached --ignore-unmatch -- \
        species.dat~    users.txt~ \
        '#species.dat#' '#users.txt#'

    git add -A *
)

# Build dev vs. live differences
export DIFFDEVLIVE=$SAVECONFDIR/diffdevlive-nag
$DIFFDEVLIVE $DATADIR > $SAVEREPO/meta/ddl.asc
$SAVECONFDIR/diffdevlive-fn -0 < $SAVEREPO/meta/ddl.asc | xargs -r0 $SAVECONFDIR/diffdevlive-diff > $SAVEREPO/meta/details.diff


TIME1=$( date +%s )

git add -A $SAVEREPO/meta

dotlockfile -c -p $LOCKFN

$SAVECONFDIR/git-qcommit -m "updated by $0, fetch took $(( $TIME1 - $TIME0 )) sec"

git push -q origin

# Force working copy to match HEAD, else we see "Changed but not updated" next time
git reset -q --hard HEAD
git clean -fdx

dotlockfile -u -p $LOCKFN
