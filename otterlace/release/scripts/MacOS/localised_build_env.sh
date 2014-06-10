#!/bin/bash

# Designed to be sourced from a script or interactive shell,
# at the top level of an otterlace build container.

. "$( dirname "${BASH_SOURCE[0]}" )/_macos_in_app.sh" || exit 1

if [ ! -d "${resources_path}" ]; then
    echo "No ./${resources_path} directory here - not an otterlace.app container?" >&2
    return 2
fi

lib_path="${PWD}/${resources_path}/lib"

export DYLD_FALLBACK_LIBRARY_PATH="${lib_path}"
export PKG_CONFIG_PATH="${lib_path}/pkgconfig"

export PERL5LIB="\
${lib_path}/perl5/site_perl:\
${lib_path}/perl5/vendor_perl:\
${lib_path}/perl5"

export PATH="\
${macos_scripts}:\
${resources_path}/bin:\
${PATH}"

unset lib_path

return $?

