#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_in_app.sh" || exit 1
lib_dir="${install_base}/lib"
make_dir="${install_base}/_non_dist"
mach_type=$(uname -m)
cd ${make_dir} &&
git clone --depth 1 git://genome-source.soe.ucsc.edu/kent.git &&
cd kent/src &&
sed -i .old "s?-lssl?${lib_dir}/libssl.a?;s?-lcrypto?${lib_dir}/libcrypto.a?" ./inc/common.mk &&
CFLAGS=-fPIC MYSQLINC=$(${install_base}/lib/percona/bin/mysql_config --include | sed 's/-I//') MYSQLLIBS=$(${install_base}/lib/percona/bin/mysql_config --libs) SSL_DIR=${install_base} PNGLIB=${lib_dir}/libpng.a ZLIB=${lib_dir}/libz.a make userApps &&
mkdir -p ${lib_dir}/kent/src/{lib,inc}
mkdir ${lib_dir}/kent/src/lib/${mach_type}
cp lib/${mach_type}/jkweb.a ${lib_dir}/kent/src/lib/${mach_type} &&
cp inc/*.h ${lib_dir}/kent/src/inc &&
cd ${make_dir} &&
rm -rf kent

exit $?
