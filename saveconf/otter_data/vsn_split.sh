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

    git filter-branch -f -d $FASTTMP --subdirectory-filter $VSN --prune-empty $VSN

    VSN_ROOT=$( git log --format=%H --max-parents=0 $VSN -- )
    case "$VSN_ROOT" in
	"$IEC") echo "Version $VSN : has the IEC\n" ;;
	'') die "Failed to get root of $VSN" ;;
	*)  # Need to graft the old IEC onto the top
	    echo "Version $VSN : root=$VSN_ROOT, grafting"
	    echo $VSN_ROOT $IEC >> .git/info/grafts
	    git filter-branch -f -d $FASTTMP $VSN
	    ;;
    esac

    rm -rf .git/refs/original $FASTTMP
}

main() {
    # Check we're in the right place - we're going to filter-branch
    git diff --quiet || die "tracked diffs"
    git diff --quiet --exit-code --cached || die "diff staged"

    [ "$( git --version )" \< "git version 1.7.9.5" ] && printf \
        "[w] git --version: Lucid and Lenny backports are too old\n    I accidentally wired in dependency on Ubuntu Precise, try\n ssh precise-dev64\n\n" >&2

    IEC=$( git log --format=%H --max-parents=0 --all )
    [ "$IEC" = '96971e864e41878425906ee80ac04880db967176' ] || die "Wrong repo (iec=$IEC)"

    printf "\n* List Otter versions\n"
    VSNS=$( git log --format=%H --all | ciids_to_vsns | sort -u | grep -E '^[0-9]+$' )
    echo Found versions $VSNS

    printf "\n* Rewriting branches for versions\n === This will leave lines in .git/info/grafts after annealing them in ===\n"
    for VSN in $VSNS; do
	do_filterbranch $VSN
    done
}

main
