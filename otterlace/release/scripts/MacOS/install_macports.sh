#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_in_app.sh" || exit 1

if [ ! -d "${resources_path}" ]; then
    echo "No ./${resources_path} directory here - setup_app_skeleton.sh not run?" >&2
    exit 2
fi

macports_url_base="https://distfiles.macports.org/MacPorts"
macports_ver="2.5.4"

macports_name="MacPorts-${macports_ver}"
macports_tarball="${macports_name}.tar.bz2"
macports_download="${macports_url_base}/${macports_tarball}"

macports_patch="${etc_macos}/macports.patch"

work_dir="${app_base}/../_macports_src"

mkdir -v -p "${work_dir}"
cd "${work_dir}"

if [ -d "${macports_name}" ]; then
    echo "Doing distclean in ${work_dir}/${macports_name}"
    cd "${macports_name}"
   [ -e Makefile ] && make distclean
else
    echo "Fetching ${macports_download}"
    curl -L "${macports_url_base}/${macports_tarball}" | tar -xjf -
    cd "${macports_name}"
fi

./configure \
  --prefix="${install_base}" \
  --with-applications-dir="${install_base}/Applications" \
  --enable-readline \
  --with-no-root-privileges

make
if [ "$(id -g)" = "$(id -gn)" ]; then
  sed -i .old 's/gname/gid/' ${work_dir}/${macports_name}/doc/base.mtree
  sed -i .old 's/gname/gid/' ${work_dir}/${macports_name}/doc/prefix.mtree
else
  id -gn | grep " " &> /dev/null
  if [ $? -eq 0 ]; then
    sed -i .old "s/gname=[[:graph:] ]* \([[:alpha:]]*=\)/gid=$(id -g) \1/" ${work_dir}/${macports_name}/doc/base.mtree
    sed -i .old "s/gname=[[:graph:] ]* \([[:alpha:]]*=\)/gid=$(id -g) \1/" ${work_dir}/${macports_name}/doc/prefix.mtree
  fi
fi
make install

${install_base}/bin/port selfupdate

exit $?

# EOF
