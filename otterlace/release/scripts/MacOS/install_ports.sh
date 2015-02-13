#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_in_app.sh" || exit 1
port_sanity_check || exit 3

ports_list="${etc_macos}/ports.list"

port_cmd="${install_base}/bin/port"

# source file is at the end of the while...
while read entry; do

    [[ $entry =~ ^# ]]             && continue # skip comment lines
    [[ $entry =~ ^[[:space:]]*$ ]] && continue # skip blank lines

    # $entry is NOT quoted as it needs to expand to individual tokens
    "${port_cmd}" install $entry

done < "${ports_list}"

# FIXME: strip local paths from ${install_base}/etc/fonts/fonts.conf

# Install into lib a link to libperl.dylib, so that it can be found via
# the DYLD_FALLBACK_LIBRARY_PATH mechanism when we relocate.

pushd "${install_base}/lib"

# YUK: hard-coded 5.12
libperl="$( find perl5 -name libperl.dylib -path '*5.12*' )"
[ -r "$libperl" ] || bail "Finding libperl.dylib failed: got '$libperl'"

ln -vsf "$libperl"

popd

exit 0
