#! /bin/bash

set -e

SRCDIR="$( unset CDPATH; cd $( dirname $0 ); pwd )"/
# needs trailing /

ARCHES=$( ls -1d /software/noarch/*/anacode/otter | cut -d/ -f4 )

DRY=-n

WHATAMI=$( cd "$SRCDIR" && git describe --abbrev=8 --tags --dirty )


mkversion() {
    printf "Copied from %s\n version %s\n\nChanges will be overwritten\n" \
        "$SRCDIR" "$WHATAMI" > $SRCDIR/version+
    mv $SRCDIR/version+ $SRCDIR/version
}

showdiff() {
    mkversion
    echo "== Diff from $SRCDIR"
    for ARCH in $ARCHES; do
        echo "==== $ARCH"
        diff -X ${SRCDIR}install.ignore -ru $SRCDIR /software/noarch/$ARCH/anacode/etc || true
    done
}

do_inst() {
    mkversion

    echo "== Installing $WHATAMI" $DRY "from $SRCDIR"
    echo "   to (" $ARCHES ")"
    for ARCH in $ARCHES; do
        echo "==== $ARCH"
        rsync -rlpg -ciSWH --delete-after \
            --chmod=ug+w,o-w,Dg+s \
            --exclude-from $SRCDIR/install.ignore $DRY \
            $SRCDIR /software/noarch/$ARCH/anacode/etc
        # rsync -a : equals -rlptgoD (no -H,-A,-X)
        #
        # These options should set the caller's default user & group, and
        # make the correct octal permissions independently of umask
    done
    echo Done.
}


while [ $# -gt 0 ]; do
    sw=$1
    shift
    case $sw in
        -d | --diff)
            showdiff
            exit 0
            ;;
        -W | --wetrun)
            DRY=
            ;;
        -h | --help)
            echo "Syntax: $0 [ --wetrun | --diff ]"
            exit 0
            ;;
        *)
            echo "Unknown option '$sw', try -h" 2>&1
            exit 1
            ;;
    esac
done

do_inst
