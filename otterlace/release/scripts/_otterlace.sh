
# Functions for inclusion in sibling scripts, with
#
#   . "$( dirname "$0" )/_otterlace.sh" || exit 1


bail() {
    echo "$1" >&2
    [ -n "$verbose" ] && _stacktrace >&2
    exit 1
}


_stacktrace_raw() {
    local n
    n=${1:-0}
    while caller $n; do
        n=$(( $n + 1 ))
    done
}
_stacktrace() {
    _stacktrace_raw ${1:-1} | sed -e 's|^\([0-9]\+\) \([^ ]\+\) \(.*\)$|   \3:\1: in \2()|'
}



config() { # deprecated, because config_get can do returncode checking for the caller
    local key
    key="$1"
    head -n 1 -- "dist/conf/${key}"
}

config_get() {
    local __varname __key __val
    __varname="$1"
    __key="${2:-$1}"

    _config_sane "config_get $__varname" "$__key" &&
    __val="$( head -n1 -- "dist/conf/$__key" )" &&
    printf -v "$__varname" '%s' "$__val" \
        || bail "config_get var=$__varname key=$__key: failed"
}

config_set() {
    local key value
    key="$1"
    value="$2"
    if [ -n "$verbose" ]; then
        printf " : config_set(%s = %s)\n" "$key" "$value"
    fi
    if _config_sane config_set "$key" "$value"; then
        sed -i -e "1s|.*|${value}|" "dist/conf/${key}"
        # returncode from sed
    else
        exit 1
        # caller may put us in a subshell, if it wants a returncode
    fi
}

_config_sane() {
    [ -f "dist/conf/$2" ] || {
        echo "dist/conf/$2 not present, cannot '$*'" >&2; return 1
    }
}


config_show_maybe() {
    local configs
#    if [ -n "$verbose" ]; then
# Useful always?
        printf "\ndist/conf/* for "
        git name-rev --always HEAD
        configs=$( cd dist/conf; echo * )
        for conf in $configs; do
            printf "  %-40s = '%s'\n" "$conf" "$(config "$conf" )"
        done
        echo
#    fi
}



git_show_maybe() {
    if [ -n "$verbose" ]; then
        git show
    fi
    true
}

git_listrefs_maybe() {
    if [ -n "$verbose" ]; then
        printf "\nTags\n"     && git tag     &&
        printf "\nBranches\n" && git branch
    fi
}

_whatami() {
    local ciid upstream
    upstream="$( cd "$dist_scripts" && git config --get remote.origin.url || echo '??' )"
    ciid="$(     cd "$dist_scripts" && git log -1 --format=%h             || echo '??' )"
    echo "$0 ($ciid from $upstream)"
}


# Assumes we were called by script in this directory.  $0 is not this file!
dist_scripts="$( dirname "$0" )"

# "What we're operating upon" is now another repo
thisprog="$( _whatami )"
