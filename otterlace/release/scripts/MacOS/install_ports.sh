#!/bin/bash

set -e # bail out on error

# FIX ME: config & path duplication

script_base="$( dirname "$0" )"

# assume we're being run in the top-level app dir, e.g. otterlace.app
app_base="${PWD}"

# based on this script being in $TT/otterlace/release/scripts/MacOS ...
etc_base="${script_base}/../../etc"

ports_list="${etc_base}/MacOS/ports.list"

resource_path="Contents/Resources"
install_base="${app_base}/${resource_path}"

port_cmd="${install_base}/bin/port"

# source file is at the end of the while...
while read entry; do
    # $entry is NOT quoted as it needs to expand to individual tokens
    "${port_cmd}" install $entry
done < "${ports_list}"

exit 0
