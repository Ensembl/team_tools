#! /bin/sh

# Assuming standard components of github.com/~mca-wtsi/git-yacontrib
# are on PATH because we plan not to need this for long

die() {
    echo "$0 abort: $@" >&2
    exit 4
}

ciids_to_vsns() {
    while read ciid; do
	git ls-tree -d --name-only $ciid
    done
}

do_filterbranch() {
    VSN=$1
    FASTTMP=/dev/shm/vsn_split
    printf "\n** filter-branch for $VSN\n"
    git branch -f --no-track $VSN origin/master

    git filter-branch -f -d $FASTTMP --prune-empty \
	--subdirectory-filter $VSN \
	--msg-filter "$MSGFILTER $VSN" \
	$VSN

    # Need to graft the old IEC onto the top
    #
    # Leaving grafts from last time doesn't help, because they aren't
    # applied by filter-branch's first run.
    printf "Version %s (%s) : filtered\n" $VSN $( git rev-parse $VSN )
    printf "%s %s" $( git log --format=%H --max-parents=0 $VSN -- ) $IEC \
        > .git/info/grafts
    git filter-branch -f -d $FASTTMP $VSN
    rm .git/info/grafts
    printf "Version %s (%s) : annealed\n" $VSN $( git rev-parse $VSN )

    rm -rf .git/refs/original $FASTTMP
}

main() {
    # Check we're in the right place - we're going to filter-branch
    git diff --quiet || die "tracked diffs"
    git diff --quiet --exit-code --cached || die "diff staged"

    [ "$( git --version )" \< "git version 1.7.9.5" ] && printf \
        "[w] git --version: Lucid and Lenny backports are too old\n    I accidentally wired in dependency on Ubuntu Precise, try\n ssh precise-dev64\n PATH=/software/perl-5.14.2/bin:\$PATH\n\n" >&2

    MSGFILTER="$( dirname $0 )/vsn_split.msg.pl"
    [ -f "$MSGFILTER" ] || die "Can't find msg-filter at $MSGFILTER"

    IEC=$( git log --format=%H --max-parents=0 --all )
    [ "$IEC" = '96971e864e41878425906ee80ac04880db967176' ] || die "Wrong repo (iec=$IEC)"

    printf "\n* List Otter versions\n"
    VSNS=$( git log --format=%H --all | ciids_to_vsns | sort -u | grep -E '^[0-9]+$|^meta' )
#VSNS="52 meta"
    echo Found versions $VSNS

    printf "\n* Rewriting branches for versions\n === This will trash .git/info/grafts , and can leave junk there if interrupted ===\n"
    for VSN in $VSNS; do
	do_filterbranch $VSN
    done
}

main
