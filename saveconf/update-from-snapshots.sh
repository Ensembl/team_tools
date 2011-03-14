#! /bin/sh

# (Attempt to) put snapshots in order
# We can swizzle the order of commits later, but that's messy and tedious

: ${TT_S:=~/gitwk--bg/team_tools}

SNAPS=$(
    (for sn in /software/noarch/.snapshot/*; do
	(find $sn/bin $sn/*/anacode/bin -printf '%CY-%Cm-%Cd %.8CT\n'
	    find $sn -maxdepth 1 -printf '%AY-%Am-%Ad %.8AT\n') \
		| sort | tail -n1
	echo -e "\t$sn"
	done) | perl -pe 's{\n}{} unless /^\t/' | sort | tee /dev/stderr | cut -f2
    )

echo >&2

for SN in $SNAPS; do

    echo Update from $SN >&2
    $TT_S/saveconf/update-project-software.sh -R $SN \
	~/gitwk--bg/project-software.saveconf anacode '*/anacode'

done
