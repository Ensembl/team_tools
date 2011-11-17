# Functions for inclusion in sibling scripts, with
#
#   . "$( dirname "$0" )/_macos.sh" || exit 1

script_base="$( dirname "$0" )"

# assume we're being run in the top-level app dir, e.g. otterlace.app
app_base="${PWD}"

# based on this script being in $TT/otterlace/release/scripts/MacOS ...
etc_macos="${script_base}/../../etc/MacOS"

resource_path="Contents/Resources"
install_base="${app_base}/${resource_path}"

# EOF

