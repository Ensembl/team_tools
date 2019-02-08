#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_in_app.sh" || exit 1
port_sanity_check || exit 3

port_cmd="${install_base}/bin/port"

# This is to remove any symlink from the mysql data
mysql_package=`${port_cmd} -q select --list mysql | grep -v none | awk '{print $1}'`
${port_cmd} select mysql none

# If you have percona installed, lib/percona/bin uses almost 200MB
rm -rf ${install_base}/lib/${mysql_package}/bin
