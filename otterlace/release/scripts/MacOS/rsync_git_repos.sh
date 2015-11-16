#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_common.sh" || exit 1

hostname="$( hostname )"
[ "$hostname" = "intcvs1" ] || bail "Must be run on intcvs1"

cd "/repos/git/anacode"

ssh_spec="otter@anacodeosx"
remote_path="~/Otter/Git/Mirrors/anacode"

# Comment these out to go live and chatty, or un-comment for testing
# dry_run="--dry-run"
verbose="--itemize-changes"

do_rsync () {
    local repo dest
    repo="$1"
    dest="$2"
    dest="${remote_path}${dest:+/$dest}"
    rsync -a --del $dry_run $verbose "$repo" "${ssh_spec}:${dest}"
}

do_rsync PerlModules.git
do_rsync ensembl-otter.git
do_rsync team_tools.git
do_rsync zircon.git
do_rsync github-mirror-ro/Ensembl/ensembl.git github-mirror-ro/Ensembl

exit $?

