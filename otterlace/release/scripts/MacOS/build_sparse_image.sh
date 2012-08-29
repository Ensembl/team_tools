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

  -h, --help            Show this help

  -n, --non_dist                Include 'non-dist' components in image.
                                (For archival purposes.)

  -r, --release <release>       Set release name. Defaults to <app_name>
                                less '.app' suffix, if any.

  -d, --detach                  Detach (unmount) the image afterwards.
${problem}"
}

detach=''
do_non_dist=''
release=''
while [ $# -gt 0 ] && [ "${1:0:1}" = "-" ]; do
    sw=$1
    shift
    case $sw in
        -h | --help)
            usage
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
        *)
            usage "unknown option $sw"
            ;;
    esac
done

[ "$#" = 1 ] || bail "Syntax: ${script_name} [ -n | --non_dist ] <app_name>"

app_name="$1"
app_name="${app_name%/}"        # strip trailing slash, if any
[ -e "${app_name}" ] || bail "${script_name}: app '${app_name}' not found"

app_stem="${app_name%.app}"     # strip trailing .app, if any
[ -z "${release}" ] && release="${app_stem}"

img_name="${release}.sparseimage"
[ ! -e "${img_name}" ] || bail "${script_name}: image '${img_name}' already exists"

if [ -n "$do_non_dist" ]; then
    non_dist_dir="$( versioned_non_dist "${app_name}" )" || exit $?
fi

echo "Creating sparseimage '${img_name}' [${release}]"
hdiutil create -fs HFS+ -volname "${release}" "${img_name}"

attach_info="$( hdiutil attach "${img_name}" )"
mount_point="$( echo "${attach_info}" | grep 'Apple_HFS' | cut -f 3 )"
echo "Mounted sparseimage at '${mount_point}'"

echo "Copying ${app_name} -> ${mount_point}"
cp -pR "${app_name}" "${mount_point}"

if [ -n "$do_non_dist" ]; then
    img_non_dist="${mount_point}/_non_dist"
    mkdir -v -p "${img_non_dist}"
    echo "Copying ${non_dist_dir} -> ${img_non_dist}"
    cp -pR "${non_dist_dir}" "${img_non_dist}"
fi

echo "Done."

if [ -n "$detach" ]; then
    hdiutil detach "${mount_point}"
fi

exit 0

# EOF
