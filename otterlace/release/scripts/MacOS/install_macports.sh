#!/bin/bash

set -e # bail out on error

# . "$( dirname "$0" )/../_otterlace.sh" || exit 1
script_base="$( dirname "$0" )"

# assume we're being run in the top-level app dir, e.g. otterlace.app
app_base="${PWD}"

macports_url_base="https://distfiles.macports.org/MacPorts"
macports_ver="2.0.3"

macports_name="MacPorts-${macports_ver}"
macports_tarball="${macports_name}.tar.bz2"
macports_download="${macports_url_base}/${macports_tarball}"

# based on this script being in $TT/otterlace/release/scripts/MacOS ...
etc_base="${script_base}/../../etc"
macports_patch="${etc_base}/MacOS/macports.patch"

resource_path="Contents/Resources"
install_base="${app_base}/${resource_path}"

work_dir="${app_base}/../_macports_src"

# ensure we don't pick up any previous MacPorts installation
export PATH=/bin:/sbin:/usr/bin:/usr/sbin

export http_proxy=http://webcache.sanger.ac.uk:3128
export HTTPS_PROXY="${http_proxy}"

mkdir -v -p "${work_dir}"
cd "${work_dir}"

if [ -d "${macports_name}" ]; then
    echo "Doing distclean in ${work_dir}/${macports_name}"
    cd "${macports_name}"
   [ -e Makefile ] && make distclean
else
    echo "Fetching ${macports_download}"
    curl -L "${macports_url_base}/${macports_tarball}" | tar -xzf -
    cd "${macports_name}"
fi

./configure \
  --prefix="${install_base}" \
  --with-applications-dir="${install_base}/Applications" \
  --enable-readline \
  --with-no-root-privileges 

make
make install

echo "Patching MacPorts config"

cd "${install_base}"
patch -p0 < "${macports_patch}"

echo
echo "Now connect to non-rsync-firewalled network and run:"
echo "  ${install_base}/bin/port selfupdate"

exit $?

# EOF
