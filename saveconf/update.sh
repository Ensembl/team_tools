#! /bin/sh

set -e

# Run with something like
#   cd ~/gitwk-anacode/server-config-trail && ~/gitwk-anacode/team_tools/saveconf/update.sh && git push -q intcvs1
#
#
# This is called every 15 minutes from cron on mca@deskpro17119 as of 2011-02-17


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


# dotlockfile is in Debian package liblockfile1
trap "dotlockfile -u -p $GIT_DIR.lock" ERR
dotlockfile -l -p $GIT_DIR.lock

mkdir -p $SAVEREPO/meta

TIME0=$( date +%s )

# List dev files
find $DATADIR -type f -print0 | LANG=en_GB.UTF-8 xargs -r0 ls -l > $SAVEREPO/meta/ls.-l

# Copy dev files to commit-staging
(
    # Anything in these $DATADIR/??  directories: add, modify, delete
    cd $DATADIR
    git add -A *
)

# Build dev vs. live differences
/nfs/WWW/bin/diffdevlive $DATADIR > $SAVEREPO/meta/ddl.asc
$SAVECONFDIR/diffdevlive-fn -0 < $SAVEREPO/meta/ddl.asc | xargs -r0 $SAVECONFDIR/diffdevlive-diff > $SAVEREPO/meta/details.diff

# Build version-to-version differences,
# for the files which should be the same
mkdir -p $SAVEREPO/derived
for leaf in species.dat users.txt; do
    {
	diff -Nsu $DATADIR/{52,53}/$leaf || true
	diff -Nsu $DATADIR/{53,54}/$leaf || true
    } > $SAVEREPO/derived/$leaf.v2v.diff
done


TIME1=$( date +%s )

git add -A $SAVEREPO/{meta,derived}

dotlockfile -c -p $GIT_DIR.lock

git commit -m "updated by $0, fetch took $[ $TIME1 - $TIME0 ] sec" | \
    (grep -Ev '^# On branch master|^nothing to commit'; true)

dotlockfile -u -p $GIT_DIR.lock
