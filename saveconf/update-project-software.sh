#! /bin/sh

set -e

# Run with something like
#   $TT_S/saveconf/update-project-software.sh [ -R /software/noarch ] <target git repo> <target git branch> <tracked globspec>+
#
#
# This would be called ... from cron on mca@deskpro17119 ...?

VERBOSE=
if [ -n "$VERBOSE" ]; then
    set -x
fi

# Whence should we slurp files?
TRACK_ROOT=/software/noarch
if  [ "$1" == '-R' ]; then
    shift
    TRACK_ROOT=$1
    shift
fi

# Find & check target Git repo + branch.  They need manual setup, and
# will be given unusual overwrite treatment.
SAVE_REPO=$1
shift
SAVE_BRANCH=$1
shift
if [ -d "$SAVE_REPO/.git" ] && [ -f "$SAVE_REPO/$SAVE_BRANCH.ignores" ]; then
    :
    # Happy with the setup
else
    echo -e "Am I in the right place?\n  SAVE_REPO=$SAVE_REPO SAVE_BRANCH=$SAVE_BRANCH\n" >&2
    ls -ld $SAVE_REPO/.git $SAVE_REPO/$SAVE_BRANCH.ignores >&2
    exit 1
    # We need a branch called $SAVE_BRANCH
    # and a branch called 'meta' containing a file called $SAVE_BRANCH.ignores
fi

TRACK_DIRS=$*


# Get up-to-date; make noise only if we fetch something
#cd $SAVE_REPO
#git fetch origin

## We don't keep a local checkout, it is too big; therefore we cannot
#  merge/pull and push (below) will fail if we needed to.
#
#git merge -q --ff-only origin/master

### if we did this on meta branch, then we should check $SAVE_BRANCH.ignores again



export GIT_DIR=$SAVE_REPO/.git
ULOCKFILE=$GIT_DIR/script.lock

# dotlockfile is in Debian package liblockfile1
trap "dotlockfile -u -p $ULOCKFILE" ERR
dotlockfile -l -p $ULOCKFILE

### Set up the repo.  Checkout only our metafiles.
#
cd $SAVE_REPO

# vape any existing _put branch
git checkout -q -f meta
git branch -D _put > /dev/null 2>&1 || true

# new _put branch, starting from our meta
git checkout -q -b _put meta

# superclean working copy
git reset -q --hard HEAD
git clean -fdqx

cp -p $VERBOSE $SAVE_REPO/$SAVE_BRANCH.ignores $GIT_DIR/info/exclude

# Now take us to where we can make a commit on $SAVE_BRANCH branch
git reset -q --mixed $SAVE_BRANCH

TIME0=$( date +%s )

# Slurp our relevant files into the index - heavy metadata access!
cd $TRACK_ROOT

# Generate a listing, and find the newest file's mtime
LATEST=$(
    find $TRACK_DIRS \
	-fprintf $SAVE_REPO/meta.ls-l '%M %2n %-8u %-8g %9s %TY-%Tm-%Td %.8TT %p\n' \
	, \
	-type f \! -newer $ULOCKFILE \
	-printf '%TY-%Tm-%Td %.8TT\n' 2> $SAVE_REPO/meta.err \
	| LC_ALL=C sort | tee $SAVE_REPO/dbg.timestamps | tail -n1 )

git add -A $TRACK_DIRS

TIME1=$( date +%s )

cd $SAVE_REPO
git add -A $SAVE_REPO/meta.{ls-l,err}

dotlockfile -c -p $ULOCKFILE

COMMENT=$( echo -e "auto-update $TRACK_DIRS from $TRACK_ROOT/\n\nby $0; add took $[ $TIME1 - $TIME0 ] sec" )
git commit -q --author 'unknown <unknown>' --date "$LATEST" -m "$COMMENT" \
    > $SAVE_REPO/dbg.lastcommit
# | \
#    (grep -Ev '^# On branch master|^nothing to commit'; true)

git branch -v -M _put $SAVE_BRANCH
git checkout -q -f meta

# git push -q origin

dotlockfile -u -p $ULOCKFILE
