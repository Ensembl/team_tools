#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_above_app.sh" || exit 1

[ "$#" = 1 ] || bail "Syntax: ${script_name} <sparse_image>"

img_name="$1"
img_name="${img_name%/}"            # strip trailing slash, if any

[ -e "${img_name}" ] || bail "${script_name}: sparse image '${img_name}' not found"

dmg_name="${img_name%.sparseimage}.dmg"
[ -e "${dmg_name}" ] && bail "${script_name}: compressed image '${dmg_name}' already exists"

attach_info="$( hdiutil attach -nomount "${img_name}" )"
mount_dev="$(   echo "${attach_info}" | grep 'Apple_HFS' | cut -f 1 | sed -e 's/ *$//' )"
mount_point="$( echo "${attach_info}" | grep 'Apple_HFS' | cut -f 3 )"
[ -n "${mount_point}" ] && bail "${script_name}: sparse image '${img_name}' still mounted at '${mount_point}'"
# The 'hdiutil attach -nomount' query leaves the image attached but unmounted, so...
hdiutil eject "${mount_dev}" > /dev/null

echo "Converting ${img_name} to ${dmg_name}"
hdiutil convert "${img_name}" -format UDBZ -o "${dmg_name}"

exit 0

# EOF
