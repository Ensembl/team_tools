
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
    local configs key val
#    if [ -n "$verbose" ]; then
# Useful always?
        printf "\ndist/conf/* for "
        git name-rev --always HEAD
        configs=$( cd dist/conf; echo * )
        for key in $configs; do
            config_get val "$key"
            printf "  %-40s = '%s'\n" "$key" "$val"
        done
        echo
#    fi
}


# Default output is "$major.$minor"
#
# Separator is always omitted if minor version is empty.
#
# Assumes the versions have the correct number of digits, and does no
# arithmetic on them.  (Beware the octal-like prefix.)
#
# Beware also that the optional minor & major args are reversed, to
# allow setting just the minor
full_version() {
    local sep pfx to_var minor major
    sep="${1:-.}"
    pfx="$2"
    to_var="${3:+-v $3}"
    minor="${4:-$( config version_minor )}"
    major="${5:-$( config version_major )}"
    printf $to_var "%s%s%s%s" "$pfx" "$major" "${minor:+$sep}" "$minor"
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

# Control of where the build is put - use it instead of hardwired
# install destinations.
#
# Writes to variable named by $1 or emits to STDOUT if $1=""
# Path type is chosen by $2 which defaults to $1
#
# Design aims: run in-line with the caller, not in a subshell; avoid
# recursion; avoid duplicated calculations; avoid array variables.
otter_ipath_get() {
    local __varname __key __out \
        version_major full_version swac nfswub \
        _oig_holtdir _oig_otter_home _oig_bin _oig_wrapperfile

    __varname="$1"
    __key="${2:-$1}"
    [ -z "$__varname" ] && __varname='__out' # will send to STDOUT

    config_get version_major
    full_version="$( full_version )" || bail "Cannot get version number"


    # These are our previously-hardcoded installation paths.
    # Accept override from the environment.
    swac=${otter_swac:-/software/anacode}
    nfswub=${otter_nfswub:-/nfs/WWWdev/SANGER_docs}

    [ -d "$swac"   ] || echo "W: swac=$swac: not a directory" >&2
    [ -d "$nfswub" ] || echo "W: nfswub=$nfswub: not a directory" >&2


    # Prepare all relevant paths once, up front...  else we're tempted
    # to recurse or duplicate the logic
    printf -v _oig_holtdir         %s/otter       "$swac"
    printf -v _oig_otter_home      %s/otter_rel%s "$_oig_holtdir" "$full_version"
    printf -v _oig_bin             %s/bin         "$_oig_otter_home"
    printf -v _oig_web_lib        %s/lib/otter/%s "$nfswub" "$version_major"
    printf -v _oig_web_cgi    %s/cgi-bin/otter/%s "$nfswub" "$version_major"

    if wrapperfile_outside_otterdir; then
        printf -v _oig_wrapperfile %s/bin/otterlace_rel%s "$swac" "$full_version"
    else
        # v58+ : wrapper in bin
        printf -v _oig_wrapperfile %s/otterlace   "$_oig_bin"
    fi


    # Do the outputting.  Also document what these are.
    case "$__key" in
        holtdir)
            # The directory in which the "otter_home"s are collected.
            # For internal Linux releases, it has been hardwired here.
            # It will become more flexible soon.
            printf -v "$__varname" %s "$_oig_holtdir" ;;
        otter_home)
            # Directory where self-contained Otterlace is installed.
            printf -v "$__varname" %s "$_oig_otter_home" ;;
        bin)
            # Directory for all binaries and scripts EXCEPT outermost
            # 'otterlace' wrapper script.  This is inside otter_home.
            printf -v "$__varname" %s "$_oig_bin" ;;
        wrapperfile)
            # Complete filename for the 'otterlace' wrapper script.
            # This needs to become version, arch & OS dependent.
            printf -v "$__varname" %s "$_oig_wrapperfile" ;;
        web_lib)
            # The directory "use SangerPaths qq{otter$major};" would give us
            printf -v "$__varname" %s "$_oig_web_lib" ;;
        web_cgi)
            # The /cgi-bin/otter/$major/ directory
            printf -v "$__varname" %s "$_oig_web_cgi" ;;
        *) bail "otter_ipath_get: unknown key '$__key'"
    esac
    [ "$__varname" = '__out' ] && printf %s "$__out"
}


# Control of the location of the outermost wrapper script...
#
# In "old" releases, $swac/bin/otterlace_rel$VSN
# in "new" releases, $OTTERHOME/bin/otterlace (with a symlink pointing in)
#
# Changing at the same time is the source for the wrapperfile.
#    In ensembl-otter commit 0fe1ed84
#    (early after humpub-release-58-dev)
#       dist/templates/otterlace moved to
#       scripts/client/otterlace
#
# Related is the split of otterlace_env.sh from otterlace (0833c7eb)
#    "dependence on $0 being the file itself not a symlink to the
#    file" arises from the sourcing of otterlace_env.sh, which is
#    commit-atomic with the split...  i.e. no need to consider that
#    here.
#
#    "otterlace_env.sh is available in the same directory as
#    otterlace" corresponds exactly with installation at
#    $OTTER_HOME/bin/otterlace .  Installing bin/otterlace outside
#    $OTTER_HOME would break it; doing it for v58+ is conservatively
#    safe.
#
# In short, where it should be installed was already decoupled from
# its origin before v57; in v58+ (specifically after 0833c7eb) it MUST
# be installed in $OTTER_HOME/bin .
#
#
# otterlace/release/scripts/otterlace_build installs it, and uses
# presence of dist/templates/otterlace in the source to decide where.
#
# otterlace/release/scripts/otterlace_symlink_update uses the presence
# of pre-existing wrapper at $swac/bin/otterlace_rel${version} , which
# follows on from the decision during otterlace_build .
#
# This function prefers not to rely on presence of ensembl-otter
# source for the decision, for automated test stability.
wrapperfile_outside_otterdir() {
    [ "$( config version_major )" -lt 58 ]
}
# There are two ways it is to be used.  Consider how it differs from
# master,
#
#    - where should otterlace_build put wrapperfile?
#      In this, we prefer to put it "inside" even for the earliest v58
#      (only relevant for rebuilding old)
#
#    - what/where should otterlace_symlink_update link?
#      It's possible an early v58 could be rebuilt using current
#      master team_tools, putting wrapperfile "outside".  This is
#      unlikely enough to ignore.



# Assumes we were called by script in this directory.  $0 is not this file!
dist_scripts="$( dirname "$0" )"

# "What we're operating upon" is now another repo
thisprog="$( _whatami )"
