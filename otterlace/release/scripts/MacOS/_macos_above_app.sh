# Functions for inclusion in scripts designed to run above the app directory, with
#
#   . "$( dirname "$0" )/_macos_above_app.sh" || exit 1

. "$( dirname "$0" )/_macos_common.sh" || exit 9

non_dist="_non_dist"
shared_non_dist="_shared"

versioned_non_dist() {
    local app_name my_non_dist_link my_non_dist_dir old_non_dist

    app_name="$1"
    my_non_dist_link="${app_name}/${resources_path}/_non_dist"
    if [ ! -e "${my_non_dist_link}" ]; then
        echo "Cannot find '${my_non_dist_link}'" >&2

        old_non_dist="$( old_links_versioned_non_dist "${app_name}" )"
        if [ -n "${old_non_dist}" ]; then
            echo "'${app_name}' looks like it needs converting to new links scheme." >&2
        else
            echo "'${app_name}' seems to have broken links." >&2
        fi

        exit 4
    fi

    my_non_dist_dir="$( cd "${my_non_dist_link}" 2>/dev/null && pwd -P )"
    if [ -z "${my_non_dist_dir}" ]; then echo "Cannot resolve link for '${my_non_dist_link}'" >&2; exit 5; fi

    echo "$( basename "${my_non_dist_dir}" )"
}

old_links_versioned_non_dist() {
    local my_non_dist_link my_non_dist_dir

    my_non_dist_link="$1/_my_non_dist"
    if [ ! -e "${my_non_dist_link}" ]; then echo "Cannot find '${my_non_dist_link}'" >&2; exit 4; fi

    my_non_dist_dir="$( cd "${my_non_dist_link}" 2>/dev/null && pwd -P )"
    if [ -z "${my_non_dist_dir}" ]; then echo "Cannot resolve link for '${my_non_dist_link}'" >&2; exit 5; fi

    echo "$( basename "${my_non_dist_dir}" )"
}

remove_links() {
    local app_base install_base
    app_base="$1"
    install_base="${app_base}/${resources_path}"

    # This is much more straightforward in the new links scheme
    #
    rm "${install_base}/_non_dist"
}

old_remove_links() {
    local app_base install_base
    app_base="$1"
    install_base="${app_base}/${resources_path}"

    rm "${app_base}/_my_non_dist"

    for f in share var; do
        rm "${install_base}/$f"
    done
}

make_links() {
    local app_base install_base non_dist_name
    app_base="$1"
    non_dist_name="$2"

    install_base="${app_base}/${resources_path}"

    # $PWD/ <-------------------\
    #   $new_app/ <----------\  |
    #     Contents <------\  |  |
    rel_inst_to_non_dist="../../../${non_dist}"

    pushd "${install_base}" > /dev/null

    echo "In ${PWD}:"
    ln -v -sf "${rel_inst_to_non_dist}/${non_dist_name}" "_non_dist"

    # Note that this does not take account of the exception list in non_dist.list.
    # This is done later by import_dist_extras.sh once the container is populated.
    #
    for f in $( non_dist_dirs ); do
        [ -e $f ] || ln -v -s "_non_dist/$f"
    done

    popd > /dev/null

}

old_make_links() {
    local app_base install_base non_dist_name
    app_base="$1"
    non_dist_name="$2"

    install_base="${app_base}/${resources_path}"

    pushd "${app_base}" > /dev/null

    echo "In ${PWD}:"
    ln -v -sf "../${non_dist}/${non_dist_name}" "_my_non_dist"

    popd > /dev/null

    # $PWD/ <-------------------\
    #   $new_app/ <----------\  |
    #     Contents <------\  |  |
    rel_inst_to_non_dist="../../../${non_dist}"

    pushd "${install_base}" > /dev/null

    echo "In ${PWD}:"
    for f in var share; do
	ln -v -sf "${rel_inst_to_non_dist}/${non_dist_name}/$f" .
    done

    popd > /dev/null

}

copy_non_dist() {
    local source dest exclude_build
    source="$1"
    dest="$2"
    exclude_build="$3"

    # FIXME: should probably be cleaning macports build dir by default.

    echo "Copying non_dist contents: ${source} => ${dest} ..."
    if [ -n "${exclude_build}" ]; then
        rsync -a --exclude '/var/macports/build/*' ${source}/{share,var} "${dest}/"
    else
        rsync -a ${source}/{share,var} "${dest}/"
    fi
}

# EOF
