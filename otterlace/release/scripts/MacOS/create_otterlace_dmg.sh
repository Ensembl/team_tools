#!/bin/bash

set -e # bail out on error

BUILD_DIR="$PWD"
APP_NAME="otterlace"
CODEBASE="$HOME/enscode"
TEAMTOOLS_DIR="$CODEBASE/team_tools"
MACOSSCRIPTS_DIR="$TEAMTOOLS_DIR/otterlace/release/scripts/MacOS"

if [ -d "${APP_NAME}.app" ]; then
  echo "setup_app_skeleton.sh has already been run"
else
  $MACOSSCRIPTS_DIR/setup_app_skeleton.sh "${APP_NAME}.app"
  if [ $? -ne 0 ];then
    echo "Failed on setup_app_skeleton.sh"
    exit 1
  fi
fi

cd "${APP_NAME}.app"

if [ -e "Contents/Resources/bin/port" ]; then
  echo "MacPort has already been installed"
else
  $MACOSSCRIPTS_DIR/install_macports.sh
  if [ $? -ne 0 ];then
    echo "Failed on install_macports.sh"
    exit 1
  fi
fi

$MACOSSCRIPTS_DIR/install_ports.sh
if [ $? -ne 0 ];then
  echo "Failed on install_ports.sh"
  exit 1
fi

if [ -e "Contents/Resources/lib/kent/src/lib/x86_64/jkweb.a" ]; then
  echo "Kent libraries have been compiled"
else
  $MACOSSCRIPTS_DIR/install_kent.sh
  if [ $? -ne 0 ];then
    echo "Failed on install_kent.sh"
    exit 1
  fi
fi

$MACOSSCRIPTS_DIR/install_cpan_bundle.sh
if [ $? -ne 0 ];then
  echo "Failed on install_cpan_bundle.sh"
  exit 1
fi

$MACOSSCRIPTS_DIR/cleanup_ports.sh
if [ $? -ne 0 ];then
  echo "Failed on cleanup_ports.sh"
  exit 1
fi

$MACOSSCRIPTS_DIR/import_dist_extras.sh
if [ $? -ne 0 ];then
  echo "Failed on import_dist_extras.sh"
  exit 1
fi

# This should not be needed anymore as to my knowledge AceDB is not used in Havana
# If it is needed, the archive can be found at ftp://ftp.sanger.ac.uk/pub/acedb/
# but it will need some work
#$MACOSSCRIPTS_DIR/install_annotools_prereqs.sh $CODEBASE
#if [ $? -ne 0 ];then
#  echo "Failed on install_annotools_prereqs.sh"
#  exit 1
#fi

$MACOSSCRIPTS_DIR/install_annotools.sh $CODEBASE
if [ $? -ne 0 ];then
  echo "Failed on install_annotools.sh"
  exit 1
fi

$MACOSSCRIPTS_DIR/install_otterlace.sh $CODEBASE/ensembl-otter
if [ $? -ne 0 ];then
  echo "Failed on install_otterlace.sh"
  exit 1
fi

cd $BUILD_DIR
$MACOSSCRIPTS_DIR/build_sparse_image.sh --detach -r otterlace_mac_intel-10.14-109 -M "${APP_NAME}.app"
if [ $? -ne 0 ];then
  echo "Failed on build_sparse_image.sh"
  exit 1
fi

$MACOSSCRIPTS_DIR/compress_image.sh otterlace_mac_intel-10.14-109.sparseimage
if [ $? -ne 0 ];then
  echo "Failed on compress_image.sh"
  exit 1
fi

rm otterlace_mac_intel-10.14-109.sparseimage
