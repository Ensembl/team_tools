#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos.sh" || exit 1

# Work out which perl lib directory to use
#
eval $( "$install_base/bin/perl" -V:version ) # sets $version
perl_ver=$version
unset version
echo "Perl version is ${perl_ver}"
perl5_lib="${install_base}/lib/perl5"

# Install a suitably-tailored version of CPAN::Config
#
config_src="${etc_macos}/CPAN/Config.pm"
config_dst="${perl5_lib}/${perl_ver}/CPAN/Config.pm"
config_bak="${config_dst}.arm_bak"

cpan_home="${install_base}/var/cpan"

[ -e "${config_dst}" ] && mv "${config_dst}" "${config_bak}"

sed -e "s|OTT_REL_MACOS_CPAN_HOME|${cpan_home}|" \
    -e "s|OTT_REL_MACOS_INSTALL_BASE|${install_base}|" \
   "${config_src}" \
 > "${config_dst}"

# Copy the distroprefs files into place
#
cpan_prefs_src="${etc_macos}/CPAN/prefs"
cpan_prefs_dst="${cpan_home}/prefs"
mkdir -v -p "${cpan_prefs_dst}"
cp -v ${cpan_prefs_src}/*.yml "${cpan_prefs_dst}"

# Copy the bundle file into place
#
bundle_src="${etc_macos}/Bundle/Otterlace/MacOS.pm"
bundle_dir="${perl5_lib}/site_perl/${perl_ver}/Bundle/Otterlace"

mkdir -v -p "${bundle_dir}"
cp -v "${bundle_src}" "${bundle_dir}"

# Use CPAN to install the bundle
#
"${install_base}/bin/cpan" -i Bundle::Otterlace::MacOS

exit 0
