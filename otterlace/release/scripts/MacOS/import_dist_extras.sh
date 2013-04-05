#!/bin/bash

set -e # bail out on error

my_dir="$( dirname "$0" )"
. "${my_dir}/_macos_in_app.sh"  || exit 1

subdivide_parent() {
    local parent tmp_parent
    parent="$1"

    tmp_parent="_tmp_import_${parent}"
    mkdir -v -p "${tmp_parent}"

    for f in $( cd "${parent}" && echo * ); do
        ln -sv "../_non_dist/${parent}/$f" "${tmp_parent}/$f"
    done

    rm "${parent}"              # get rid of existing link
    mv -v "${tmp_parent}" "${parent}"
}

(
    pushd "${install_base}" > /dev/null
    echo "In ${PWD}:"

    for e in $( non_dist_exceptions ); do

        # This assumes that $e is of form parent/child with only one /
        parent="$( dirname "$e" )"
        child="$( basename "$e" )"

        # We expect the parent to already be a link into _non_dist.
        # Otherwise we've probably already been run.
        #
        if [ -L "${parent}" ]; then

            parent_in="$( dirname $( readlink "${parent}" ) )"

            if [ $parent_in = '_non_dist' ]; then
                subdivide_parent "${parent}"
            else
                bail "'${parent}' is symlink into '${parent_in}', cannot cope!"
            fi

        else
            echo "'${parent}' is a real directory, assuming it has already been materialised."
        fi

        pushd "${parent}" > /dev/null

        echo "In ${PWD}:"

        if [ -L "${child}" ]; then
            src="$( readlink "${child}" )"
            tmp="_tmp_import_${child}"
            echo "Copying contents of '${child}' from '${src}'"
            cp -a "${src}" "${tmp}"
            rm "${child}"       # get rid of link
            mv "${tmp}" "${child}"
        else
            echo "'${child}' is already present in '$( readlink "${PWD}" )'"
        fi

        popd > /dev/null

    done

    popd > /dev/null
)

exit 0
