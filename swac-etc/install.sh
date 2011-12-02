#! /bin/bash

set -e

SRCDIR="$( dirname $0 )"/
# needs trailing /

ARCHES=$( ls -1d /software/noarch/*/anacode/otter | cut -d/ -f4 )

DRY=-n

while [ $# -gt 0 ]; do
    sw=$1
    shift
    case $sw in
        -W | --wetrun)
            DRY=
            ;;
        -h | --help)
            echo "Syntax: $0 [ --wetrun ]"
            exit 0
            ;;
        *)
            echo "Unknown option '$sw', try -h" 2>&1
            exit 1
            ;;
    esac
done


WHATAMI=$( cd "$SRCDIR" && git describe --abbrev=8 --tags --dirty )
echo "Copied from $SRCDIR version $WHATAMI" > $SRCDIR/version+
mv $SRCDIR/version+ $SRCDIR/version


echo "== Installing $WHATAMI" $DRY "from $SRCDIR"
echo "   to (" $ARCHES ")"
for ARCH in $ARCHES; do
    echo "==== $ARCH"
    rsync -rlpg -ciSWH --delete-after \
        --chmod=ug+w,o-w \
        --exclude-from $SRCDIR/install.ignore $DRY \
        $SRCDIR /software/noarch/$ARCH/anacode/etc
    # rsync -a : equals -rlptgoD (no -H,-A,-X)
    #
    # These options should set the caller's default user & group, and
    # make the correct octal permissions independently of umask
done
echo Done.
