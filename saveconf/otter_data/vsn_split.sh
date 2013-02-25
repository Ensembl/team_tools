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
    VSNS="$2"

    # it came with newlines; index-filter needs spaces
    VSNS="$( echo $VSNS )"

    FASTTMP=/dev/shm/vsn_split
    printf "\n** filter-branch for $VSN\n"
    git checkout -q $IEC
    git branch -f --no-track $VSN origin/master

    if [ "$VSN" = 'root' ]; then
        git filter-branch -f -d $FASTTMP --prune-empty \
            --index-filter "git rm -rq --cached --ignore-unmatch $VSNS derived .gitignore ls.-l" \
            --parent-filter "perl $PARFILTER $VSN $IEC" \
            --msg-filter "perl $MSGFILTER $VSN" \
            $VSN
    else
        git filter-branch -f -d $FASTTMP --prune-empty \
            --subdirectory-filter $VSN \
            --parent-filter "perl $PARFILTER $VSN $IEC" \
            --msg-filter "perl $MSGFILTER $VSN" \
            $VSN
    fi || die "filtering failed"

    printf "Version %s (%s) : filtered\n" $VSN $( git rev-parse $VSN )

    rm -rf .git/refs/original $FASTTMP
}

main() {
    # Check we're in the right place - we're going to filter-branch
    git diff --quiet || die "tracked diffs"
    git diff --quiet --exit-code --cached || die "diff staged"

    [ "$( git --version )" \< "git version 1.7.9.5" ] && printf \
        "[w] git --version: Lucid and Lenny backports are too old\n    I accidentally wired in dependency on Ubuntu Precise, try\n ssh precise-dev64\n PATH=/software/perl-5.14.2/bin:\$PATH\n\n" >&2

    MSGFILTER="$( dirname $0 )/vsn_split.msg.pl"
    PARFILTER="$( dirname $0 )/vsn_split.par.pl"
    [ -f "$MSGFILTER" ] || die "Can't find msg-filter at $MSGFILTER"
    [ -f "$PARFILTER" ] || die "Can't find par-filter at $PARFILTER"

    printf "\n* IEC detection\n === This will trash .git/info/grafts , and can leave junk there if interrupted ===\n"
    # grafts can break IEC detection
    rm -fv .git/info/grafts
    IEC=$( git log --format=%H --max-parents=0 --all )
    [ "$IEC" = '96971e864e41878425906ee80ac04880db967176' ] || die "Wrong repo (iec=$IEC)"

    printf "\n* List Otter versions\n"
    VSNS=$( git log --format=%H origin/master | ciids_to_vsns | sort -u | grep -E '^[0-9]+$|^meta' )
#VSNS="52 meta"
    echo Found versions $VSNS

    printf "\n* Rewriting branches for versions\n"
    for VSN in $VSNS root; do
	do_filterbranch $VSN "$VSNS" || die "filterbranch fail on $VSN"
    done

    printf "\n* Merging\n"
    perl "$( dirname $0 )/vsn_merge.pl" || die "vsn_merge.pl fail"

    printf "\n* Pushy\n"
    if git remote show derived >/dev/null; then
        VSNS_LEFT=$( git branch | perl -ne 's{^[ *]+}{}; print if /^(\d+|meta|root)$/' )
        VSNS_GONE=$( git tag -l | perl -ne 'print qq{$1\n} if m{^rm/(\d+)$}' )
        printf "Versions gone: %s\n    remaining: %s\n\n" \
            "$( echo $VSNS_GONE )" "$( echo $VSNS_LEFT )"
        GONE_PUSH=$( perl -e 'print map {"rm/$_:refs/heads/$_\n"} @ARGV' $VSNS_GONE )

        # push them all at once, else the repo merges for us; then we
        # can't f-f
        git push derived $GONE_PUSH $VSNS_LEFT live_new:dev live_new:live
    else
        printf "Remote 'derived' not defined - skipping\n"
    fi
}

main
