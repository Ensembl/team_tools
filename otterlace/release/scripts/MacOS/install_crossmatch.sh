#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_in_app.sh"  || exit 1
port_sanity_check || exit 3

check_set_zmap_build_dir_from_arg "$@"

cd ${install_base}/_non_dist

# Cleanup
echo "Removing previous cross_match builds, if any"
rm -rf phrap-*

phrap_tarball="${zmap_dist_dir}/phrap-*.tgz"
phrap_dir=`basename $(echo ${phrap_tarball} | sed 's/.tgz//')`
mkdir ${phrap_dir}
tar -C ${PWD}/${phrap_dir} ${phrap_tarball}

cd ${phrap_dir}
make CC=${install_base}/bin/gcc

cp 
PAM250 \
BLOSUM62 \
BLOSUM50 \
mat50 \
mat70 \
mb_matrix \
penalty2 \
swat \
phrap \
cross_match \
phrapview \
cluster \
loco \
calf_merge \
${install_base}/bin/

exit $?
