#!/bin/bash

set -e # bail out on error

. "$( dirname "$0" )/_macos_above_app.sh" || exit 1

usage() {
    local problem
    problem=''
    [ -n "$1" ] && problem="\n${script_name}: ${1}\n"

    bail "Syntax: ${script_name} [ options ] <app_name>

  This should be run from the dist directory containing <app_name>
  (and also containing _non_dist/<app_name>.<serial> if -n option).

  Options:

  -h, --help                    Show this help

  -m, --read_me <dir>           Copy ReadMe directory into top of image.

  -M, --e_o_read_me             Find ReadMe directory via footprints
                                left by install_otterlace.sh, and
                                copy into top of image.

  -n, --non_dist                Include 'non-dist' components in image.
                                (For archival purposes.)

  -r, --release <release>       Set release name. Defaults to <app_name>
                                less '.app' suffix, if any.

  -d, --detach                  Detach (unmount) the image afterwards.

  -x, --exclude_build           Exclude var/macports/build/*
                                (Only permitted with --non_dist)
${problem}"
}

detach=''
do_non_dist=''
exclude_build=''
release=''
read_me=''
e_o_read_me=''

while [ $# -gt 0 ] && [ "${1:0:1}" = "-" ]; do
    sw=$1
    shift
    case $sw in
        -h | --help)
            usage
            ;;
        -m | --read_me)
            read_me=$1
            [ -d "${read_me}" ] || bail "'${read_me}' not found"
            shift
            ;;
        -M | --e_o_read_me)
            e_o_read_me=1
            ;;
        -n | --non_dist)
            do_non_dist=1
            ;;
        -r | --release)
            release=$1
            [ -n "${release}" ] || usage "${sw} flag must be followed by release name"
            shift
            ;;
        -d | --detach)
            detach=1
            ;;
        -x | --exclude_build)
            exclude_build=1
            ;;
        *)
            usage "unknown option $sw"
            ;;
    esac
done

[ -n "${read_me}" ]       && [ -n "${e_o_read_me}" ] && usage "--read_me and --e_o_readme are mutually exclusive"
[ -n "${exclude_build}" ] && [ -z "${do_non_dist}" ] && usage "--exclude_build requires --non_dist"
[ "$#" = 1 ] || usage "Must provide a single argument: <app_name>"

app_name="$1"
app_name="${app_name%/}"        # strip trailing slash, if any
[ -e "${app_name}" ] || bail "${script_name}: app '${app_name}' not found"

non_dist_dir="${non_dist}/$( versioned_non_dist "${app_name}" )"
if [ -n "$do_non_dist" ]; then
    [ -d "$non_dist_dir" ] || bail "non_dist_dir '${non_dist_dir}' not found"
fi

if [ -n "${e_o_read_me}" ]; then
    footprint="${non_dist_dir}/_ensembl_otter"
    [ -L "$footprint" ] || bail "'${footprint}' not found for ReadMe"
    read_me="$( readlink "$footprint" )/docs/ReadMe.rtfd"
    [ -d "${read_me}" ] || bail "'${read_me}' not found"
fi

app_stem="${app_name%.app}"     # strip trailing .app, if any
[ -z "${release}" ] && release="${app_stem}"

img_name="${release}.sparseimage"
[ ! -e "${img_name}" ] || bail "${script_name}: image '${img_name}' already exists"

echo "Creating sparseimage '${img_name}' [${release}]"
hdiutil create -fs HFS+ -volname "${release}" "${img_name}"

attach_info="$( hdiutil attach "${img_name}" )"
mount_point="$( echo "${attach_info}" | grep 'Apple_HFS' | cut -f 3 )"
echo "Mounted sparseimage at '${mount_point}'"

echo "Copying ${app_name} -> ${mount_point}"
cp -pR "${app_name}" "${mount_point}"

echo "Fixing binary install names in ${mount_point}/${app_name}"
"${macos_scripts}/fix_binary_install_names" "${mount_point}/${app_name}"

if [ -n "$do_non_dist" ]; then
    img_non_dist="${mount_point}/${non_dist_dir}"
    mkdir -v -p "${img_non_dist}"
    copy_non_dist "${non_dist_dir}" "${img_non_dist}" "${exclude_build}"
fi

if [ -n "$read_me" ]; then
    echo "Copying ${read_me} -> ${mount_point}/"
    cp -pR "${read_me}" "${mount_point}"
fi

if [ -n "$detach" ]; then
    echo "Detaching ${mount_point}"
    hdiutil detach "${mount_point}"
fi

echo "Done."

exit 0

# EOF
