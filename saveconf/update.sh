#! /bin/sh

set -e

# Run with something like
#   cd ~/gitwk-anacode/server-config-trail && ~/gitwk-anacode/saveconf/update.sh


SAVEREPO=$PWD
if [ -d "$SAVEREPO/.git" ] && [ $( basename $SAVEREPO ) == "server-config-trail" ]; then
    :
    # Happy with PWD being our SAVEREPO
else
    echo "$0: Are we in the right directory?  SAVEREPO=$SAVEREPO" >&2
    echo "Expected this to have .../server-config-trail/.git" >&2
    exit 1
fi

SAVECONFDIR=$( dirname $0 )
DATADIR=/nfs/WWWdev/SANGER_docs/data/otter

export GIT_DIR=$SAVEREPO/.git

mkdir -p -v $SAVEREPO/meta

TIME0=$( date +%s )

# List dev files
find $DATADIR -type f -print0 | xargs -r0 ls -l > $SAVEREPO/meta/ls.-l

# Copy dev files to commit-staging
(
    # Anything in these $DATADIR/??  directories: add, modify, delete
    cd $DATADIR
    git add -A *
)

# Build dev vs. live differences
/nfs/WWW/bin/diffdevlive $DATADIR > $SAVEREPO/meta/ddl.asc
$SAVECONFDIR/diffdevlive-fn -0 < $SAVEREPO/meta/ddl.asc | xargs -r0 $SAVECONFDIR/diffdevlive-diff > $SAVEREPO/meta/details.diff

git add -A $SAVEREPO/meta
TIME1=$( date +%s )

git commit -m "updated by $0, fetch took $[ $TIME1 - $TIME0 ] sec" | \
    (grep -Ev '^# On branch master|^nothing to commit'; true)
