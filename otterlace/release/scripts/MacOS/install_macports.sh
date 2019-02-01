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

echo "Patching MacPorts config"

cd "${install_base}"
patch -p0 < "${macports_patch}"

macports_sources_conf="${install_base}/etc/macports/sources.conf"
local_ports_src="${etc_macos}/Ports"
local_ports_dst="${install_base}/etc/local_ports"

sed -i ".pre-sed" \
    -e "s|OTT_REL_MACOS_LOCAL_PORTS|${local_ports_dst}|" \
   "${macports_sources_conf}"

echo "Installing local ports files"

mkdir -v -p "${local_ports_dst}"
cp -v -p -R ${local_ports_src}/* "${local_ports_dst}"

port_update_dst="${install_base}/sbin/port_update.sh"
cp -v -p "${etc_macos}/port_update.sh.template" "${port_update_dst}"
chmod +x "${port_update_dst}"

echo "Updating ports lists"
${port_update_dst}

echo
echo "Ports lists have been updated."
echo
echo "To update again later, run:"
echo "  ${port_update_dst}"

exit $?

# EOF
