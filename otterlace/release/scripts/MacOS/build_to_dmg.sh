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
clone_from_master=
build_app=
target_app=
parked_app=
park_to=

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

  --target_stem       <stem>    Stem of the target app (default: '${target_stem}')
                                Defaults for further options are set from this.

  --parked_app        <app>     The parked app to use for the build, unless:
  --clone_from_master <app>     Clone from named master app

  --build_app         <app>     The app directory to build in
  --target_app        <app>     The target app directory for the image

  --image_stem        <stem>    Stem for image names
  --release_tag       <version> Release tag (default: taken from --otter)

  --install_annotools_prereqs   Install annotools prerequisites
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
            --clone_from_master)
                clone_from_master=$1
                shift
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

    target_app="${target_app:-${target_stem}.app}"
    build_app="${build_app:-build_${target_app}}"
    parked_app="${parked_app:-${build_app/.app/_parked.app}}"

    image_stem="${image_stem:-${target_stem}_mac_intel}"

    true
}


can_untouch() {
    local what desc
    what="$1"
    desc="$2"

    [ -e "$what" ] || return 0

    if [[ -f "$what" && ! -s "$what" ]]; then
        chat "${desc}: can delete placeholder '$what'"
    else
        bail "${desc}: will not delete non-placeholder '$what'"
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

    if [ -n "${clone_from_master}" ]; then
        [ -d "${clone_from_master}" ] || bail "--clone_from_master: '${clone_from_master}' not found"
        park_to="${build_app}.$( date "+%FT%T" )"
    else
        # We're using parked (the default)
        [ -d "${parked_app}" ]        || bail "--parked_app '${parked_app}' not found"
        park_to="${parked_app}"
    fi

    can_untouch "${build_app}" "--build_app"
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

    [ -e "$sparse_image" ] && bail "'${sparse_image}' is in the way"
    [ -e "$compressed_image" ] && bail "'${compressed_image}' is in the way"

    true
}


display_options() {
    [[ -n "$verbose" || -n "$display_only" ]] || return 0

    local mode detail
    if [ -n "$clone_from_master" ]; then
        mode="clone_from_master: ${clone_from_master}"
        detail="(not using --parked_app)"
    else
        mode="parked_app:        ${parked_app}"
        detail="(not using --clone_from_master)"
    fi

    echo "Settings:
    otter:             ${otter}
    zmap:              ${zmap}

    target_stem:       ${target_stem}

    ${mode}
                       ${detail}

    build_app:         ${build_app}
    target_app:        ${target_app}
    park_to:           ${park_to}

    image_stem:        ${image_stem}
    release_tag:       ${release_tag}
    sparse_image:      ${sparse_image}
    compressed_image:  ${compressed_image}

    annotools_prereqs: ${annotools_prereqs:-no}
    skip_annotools:    ${skip_annotools:-no}
"
    [ -n "$display_only" ] && exit
    true
}


prepare() {
    chat "prepare"
    [ -e "$build_app"  ]        && rm -v "$build_app"
    [ -e "$target_app" ]        && rm -v "$target_app"
    [ -z "$clone_from_master" ] && "${macos_scripts}/rename_app.sh" "$parked_app" "$build_app"
    true
}


clone_master() {
    [ -n "$clone_from_master" ] || return 0
    chat "clone_master"
    "${macos_scripts}/clone_app.sh" "$clone_from_master" "$build_app"
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

    "${macos_scripts}/rename_app.sh" "$target_app" "$park_to"
    touch "$target_app"
    tell "touched '${target_app}'"
}


compress_dmg() {
    chat "compress_dmg"
    ${dry_run:+echo DRY_RUN:} "${macos_scripts}/compress_image.sh" "$sparse_image"
}


cleanup() {
    chat "cleanup"
    ${dry_run:+echo DRY_RUN:} rm -v "$sparse_image"
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
