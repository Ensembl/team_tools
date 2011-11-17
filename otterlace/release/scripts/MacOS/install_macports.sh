#!/bin/bash

set -e # bail out on error

# . "$( dirname "$0" )/../_otterlace.sh" || exit 1

macports_url_base="https://distfiles.macports.org/MacPorts"
macports_ver="2.0.3"
macports_name="MacPorts-${macports_ver}"
macports_tarball="${macports_name}.tar.bz2"

install_base="${PWD}"
work_dir="${install_base}/_macports_src"

# ensure we don't pick up any previous MacPorts installation
export PATH=/bin:/sbin:/usr/bin:/usr/sbin

export http_proxy=http://webcache.sanger.ac.uk:3128
export HTTPS_PROXY="${http_proxy}"

mkdir -v -p "${work_dir}"
cd "${workdir}"

curl -L "${macports_url_base}/${macports_tarball}" | tar -xzf -
cd "${macports_name}"

./configure \
  --prefix="${install_base}" \
  --with-applications-dir="${install_base}/Applications" \
  --enable-readline \
  --with-no-root-privileges 

make
make install

echo "Need to modify config here"
echo
echo "Now connect to non-rsync-firewalled network and run:"
echo "  ${install_base}/bin/port selfupdate"

exit $?

