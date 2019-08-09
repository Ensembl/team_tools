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

# This is to allow mysql_config to be in the PATH
"${port_cmd}" select mysql $(${port_cmd} -q select --list mysql | grep -v none | awk '{print $1}')

exit 0
