#!/bin/bash

set -e # bail out on error
#set -x

. "$( dirname "$0" )/_macos_above_app.sh" || exit 1


# Defaults
#
verbose=
dry_run=
display_only=

otter=
zmap=
annotools_prereqs=
release_tag=

target_stem="otterlace"
master_app=
no_clone=
build_app=
cleanup_build_app=
target_app=
parked_app=
use_parked=

image_stem=
image_release=
sparse_image=
compressed_image=

skip_annotools=

usage() {
    local problem
    problem=''
    [ -n "$1" ] && problem="\n${script_name}: ${1}\n"

    bail "Syntax: ${script_name} -otter <ensembl-otter> --zmap <zmap-build> ...

  This should be run from the directory containing the master
  otterlace app directory.

  Required arguments:

  --otter <ensembl-otter>       Path to the ensembl-otter to be installed
  --zmap  <zmap-build>          Path to the ZMap distribution to be installed

  Options:

  -h, --help                    Show this help
  -v, --verbose                 Be chatty
  -n, --dry-run                 Skip actual builds, echoing what would be done
  -d, --display_only            Exit after checking and displaying settings

  --install_annotools_prereqs   Install annotools prerequisites
  --release_tag                 Release tag (default: taked from --otter)

  --target_stem <stem>          Stem of the target app (default: '${target_stem}')
  --master_app  <dirname>       Name of the master app directory to be cloned
  --no_clone                    Do not make a clone (must set --build_app)
  --build_app                   The app directory to build in (if --no_clone)
  --target_app                  The target app directory for the image
  --parked_app                  Where to park the app after the image is created
  --use_parked                  Use parked app for this build (implies --no_clone)

  --image_stem                  Stem for image names (defaults to --target_stem)

  --skip_annotools              Don't rebuild annotools

${problem}"
}


tell() {
    echo -e "${script_name}: $1"
}

chat() {
    [ -n "$verbose" ] && tell "$1"
    true
}


process_options() {
    while [ $# -gt 0 ] && [ "${1:0:1}" = "-" ]; do
        sw=$1
        shift
        case $sw in
            -h | --help)
                usage
                ;;
            -v | --verbose)
                verbose=1
                ;;
            -n | --dry-run)
                dry_run=1
                ;;
            -d | --display_only)
                display_only=1
                ;;
            --otter)
                otter=$1
                shift
                ;;
            --zmap)
                zmap=$1
                shift
                ;;
            --install_annotools_prereqs)
                annotools_prereqs=INSTALL
                ;;
            --release_tag)
                release_tag=$1
                shift
                ;;
            --target_stem)
                target_stem=$1
                shift
                ;;
            --master_app)
                master_app=$1
                shift
                ;;
            --no_clone)
                no_clone=1
                ;;
            --build_app)
                build_app=$1
                shift
                ;;
            --target_app)
                target_app=$1
                shift
                ;;
            --parked_app)
                parked_app=$1
                shift
                ;;
            --use_parked)
                use_parked=YES
                no_clone=1
                ;;
            --image_stem)
                image_stem=$1
                shift
                ;;
            --skip_annotools)
                skip_annotools=SKIP
                ;;
            *)
                usage "unknown option $sw"
                ;;
        esac
    done

    # option inter-dependencies go here

    chat "processed options"
}


expand_defaults() {
    chat "expand_defaults"

    master_app="build_${target_stem}_master.app"
    build_app="${build_app:-${master_app/_master/}}"
    target_app="${target_app:-${target_stem}.app}"
    parked_app="${parked_app:-${master_app/_master/_parked}}"

    image_stem="${image_stem:-${target_stem}}"

    true
}


can_untouch() {
    local what desc
    what="$1"
    desc="$2"

    [ -e "$what" ] || return 0

    if [[ -f "$what" && ! -s "$what" ]]; then
        chat "${desc}: can delete '$what'"
    else
        bail "${desc}: will not delete '$what'"
    fi
    true
}


check_prereqs() {
    chat "check_prereqs"

    [ -d "_non_dist" ] || bail "_non_dist not found: need to be run in suitable Dist directory"

    [ -n "$otter" ] || bail "--otter must be set"
    [ -d "$otter" ] || bail "--otter: '${otter}' not found"
    [ -n "$zmap" ]  || bail "--zmap must be set"
    [ -d "$zmap" ]  || bail "--zmap: '${zmap}' not found"

    if [ -n "${no_clone}" ]; then
        # We're not cloning
        if [ -n "${use_parked}" ]; then
            [ -d "${parked_app}" ] || bail "--parked_app: '${parked_app}' not found"
            cleanup_build_app=1
        else
            [ -d "${build_app}" ] || bail "--build_app: '${build_app}' not found"
        fi
    else
        # We are cloning
        [ -d "${master_app}" ] || bail "--master_app: '${master_app}' not found"
        cleanup_build_app=1
    fi

    [ -n "$cleanup_build_app" ] && can_untouch "${build_app}" "--build_app"
    can_untouch "${target_app}" "--target_app"

    true
}


get_release_tag() {
    if [ -z "$release_tag" ]; then
        release_tag="$( source "${macos_scripts}/../_otterlace.sh" && cd "$otter" && full_version )"
        tell "Got release_tag '${release_tag}' from '${otter}'"
    fi
    image_release="${image_stem}-${release_tag}"
    sparse_image="${image_release}.sparseimage"
    compressed_image="${image_release}.dmg"
    true
}


display_options() {
    [[ -n "$verbose" || -n "$display_only" ]] || return 0

    local clone
    if [ -n "$no_clone" ]; then clone="--no_clone => not using master_app"; else clone="$master_app"; fi

    echo "Settings:
    otter:             ${otter}
    zmap:              ${zmap}
    release_tag:       ${release_tag}
    annotools_prereqs: ${annotools_prereqs:-no}
    skip_annotools:    ${skip_annotools:-no}

    target_stem:       ${target_stem}
    master_app:        ${clone}
    build_app:         ${build_app}
    target_app:        ${target_app}
    parked_app:        ${parked_app}
    use_parked:        ${use_parked:-no}

    image_stem:        ${image_stem}
    sparse_image:      ${sparse_image}
    compressed_image:  ${compressed_image}
"
    [ -n "$display_only" ] && exit
    true
}


prepare() {
    chat "prepare"
    [[ -n "$cleanup_build_app" && -e "$build_app" ]] && rm -v "$build_app"
    [ -e "$target_app" ] && rm -v "$target_app"
    [ -n "$use_parked" ] && "${macos_scripts}/rename_app.sh" "$parked_app" "$build_app"
    true
}


clone_master() {
    [ -z "$no_clone" ] || return 0
    chat "clone_master"
    "${macos_scripts}/clone_app.sh" "$master_app" "$build_app"
}


clean_before_build() {
    chat "clean_before_build"

    local otter ng_changed
    shopt -u | grep -q nullglob && ng_changed=true && shopt -s nullglob

    for otter in $build_app/Contents/Resources/otter/*; do
        tell "Removing '$otter'"
        ${dry_run:+echo DRY_RUN:} rm -r "$otter"
    done

    [ $ng_changed ] && shopt -u nullglob

    true
}


set_env_for_build_app() {
    chat "set_env_for_build_app"
    pushd "$build_app"
    source "${macos_scripts}/localised_build_env.sh"
    chat "working in '$PWD'"
}


install_annotools_prereqs() {
    [ -n "$annotools_prereqs" ] || return 0
    chat "install_annotools_prereqs"
    ${dry_run:+echo DRY_RUN:} "${macos_scripts}/install_annotools_prereqs.sh" "$zmap"
}


install_annotools() {
    [ -n "$skip_annotools" ] && return 0
    chat "install_annotools"
    ${dry_run:+echo DRY_RUN:} "${macos_scripts}/install_annotools.sh" "$zmap"
}


install_otterlace() {
    chat "install_otterlace"
    DISPLAY= ${dry_run:+echo DRY_RUN:} "${macos_scripts}/install_otterlace.sh" "$otter"
}


done_in_build_app() {
    chat "done in '$build_app'"
    popd
}


rename_build_app() {
    chat "rename_build_app"
    "${macos_scripts}/rename_app.sh" "$build_app" "$target_app"
    touch "$build_app"
    tell "touched '${build_app}'"
}


build_sparse_image() {
    chat "build_sparse_image"
    ${dry_run:+echo DRY_RUN:} "${macos_scripts}/build_sparse_image.sh" \
                                --e_o_read_me --release "$image_release" --detach  "$target_app"
}


park_build_app() {
    chat "park_build_app"
    "${macos_scripts}/rename_app.sh" "$target_app" "$parked_app"
    touch "$target_app"
    tell "touched '${target_app}'"
}


compress_dmg() {
    chat "compress_dmg"
    ${dry_run:+echo DRY_RUN:} "${macos_scripts}/compress_image.sh" "$sparse_image"
}


cleanup() {
    chat "cleanup"
}


main() {
    echo "${script_name}: starting"

    process_options "$@"
    expand_defaults
    check_prereqs
    get_release_tag
    display_options

    prepare
    clone_master
    clean_before_build
    set_env_for_build_app

    install_annotools_prereqs
    install_annotools
    install_otterlace
    done_in_build_app

    rename_build_app
    build_sparse_image
    park_build_app
    compress_dmg

    cleanup
    echo "${script_name}: done"
}


# Let's do it!

main "$@"
exit $?


# EOF
