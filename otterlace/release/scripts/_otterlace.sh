
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

config_get_maybe() {
    local __varname __key __val __default
    __varname="$1"
    __key="${2:-$1}"
    __default="$3"

    if _config_sane "config_get_maybe $__varname" "$__key" "silent"; then
        __val="$( head -n1 -- "dist/conf/$__key" )" &&
        printf -v "$__varname" '%s' "$__val"
    else
        printf -v "$__varname" '%s' "$__default"
    fi
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
        [ -n "$3" ] && return 1
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
    git --version >/dev/null || bail "Git is broken, cannot continue.  PATH=$PATH"
    upstream="$( cd "$dist_scripts" && git config --get remote.origin.url || echo '??' )"
    descr="$(    cd "$dist_scripts" && git describe --tags --long --match 'otter/*' || echo '??' )"
    echo "$0 ($descr from $upstream)"
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
        version_major version_minor full_version swac nfswub otterhome_suffixing \
        _oig_feature _oig_sfxfeat \
        _oig_holtdir _oig_otter_home _oig_bin _oig_wrapperfile

    __varname="$1"
    __key="${2:-$1}"
    [ -z "$__varname" ] && __varname='__out' # will send to STDOUT

    # This can go once otterlace has disappeared beyond otterlace_old.
    config_get_maybe otter_name "" "otterlace"

    config_get version_major
    config_get version_minor
    _oig_feature="$( _feature_get )"
    _oig_majfeat="${version_major}${_oig_feature:+_}$_oig_feature"
    full_version="$version_major"
    [ -n "$version_minor" ] && full_version="${full_version}.${version_minor}"
    [ -n "$_oig_feature" ] && full_version="${full_version}_${_oig_feature}"

    if [ -n "$otter_suffix" ]; then
        # override from environment; slightly deprecated, but probably
        # useful enough to keep
        otterhome_suffixing="$otter_suffix"
    else
        config_get otterhome_suffixing
    fi

    # These are our previously-hardcoded installation paths.
    # Accept override from the environment.
    swac=${otter_swac:-/software/anacode}
    nfswub=${otter_nfswub:-/nfs/anacode/WEBVM_docs.dev}

    [ -d "$swac"   ] || echo "W: swac=$swac: not a directory" >&2
    [ -d "$nfswub" ] || [ -n "$local_client_only" ] || echo "W: nfswub=$nfswub: not a directory" >&2


    # Prepare all relevant paths once, up front...  else we're tempted
    # to recurse or duplicate the logic
    printf -v _oig_holtdir         %s/otter       "$swac"
    printf -v _oig_otter_home      %s/otter_rel%s "$_oig_holtdir" "$full_version"
    printf -v _oig_bin             %s/bin         "$_oig_otter_home"
    printf -v _oig_web_lib        %s/lib/otter/%s "$nfswub" "$_oig_majfeat"
    printf -v _oig_web_cgi    %s/cgi-bin/otter/%s "$nfswub" "$_oig_majfeat"
    printf -v _oig_web_psgi      %s/psgi/otter/%s "$nfswub" "$_oig_majfeat"
    printf -v _oig_wrapperfile     %s/%s          "$_oig_bin" "${otter_name}"

    if [[ "$otterhome_suffixing" =~ 'arch' ]]; then
        # When building to non-/software NFS, it is useful to include
        # the arch
        printf -v _oig_otter_home %s-%s "$_oig_otter_home" "$(uname -m )"
    fi
    if [[ "$otterhome_suffixing" =~ 'distro' ]]; then
        # While between OS versions, it is useful to include the OS
        # codename
        printf -v _oig_otter_home %s-%s "$_oig_otter_home" "$( lsb_release -sc )"
    fi


    # Do the outputting.  Also document what these are.
    case "$__key" in
        swac)
            # The root of our software installs
            printf -v "$__varname" %s "$swac" ;;
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
        web_psgi)
            # The /psgi/otter/$major/ directory
            printf -v "$__varname" %s "$_oig_web_psgi" ;;
        feature)
            # The name of the feature branch, or empty string if none
            printf -v "$__varname" %s "$_oig_feature" ;;
        majfeat)
            # ${version_major} or ${version_major}_${feature}
            # as used with cgi-bin/otter/$majfeat
            printf -v "$__varname" %s "$_oig_majfeat" ;;
        *) bail "otter_ipath_get: unknown key '$__key'"
    esac
    [ "$__varname" = '__out' ] && printf %s "$__out"
}


_feature_get() {
    if git symbolic-ref -q HEAD >/dev/null; then
        # we are on a branch
        git branch |
        sed -n -e 's|^\*[[:blank:]]\+feature/\([-_[:alnum:]]\+\).*$|\1|p'
    else
        bail "With a detached HEAD, would not know if this should be a feature branch"
    fi
}


# Call with a variable to write and a list of environment variable
# names, which must have been exported.  Absent variables are ignored.
#
# Generates a string suitable for passing as arguments to env.
#
# If it cannot guarantee repeatable quote safety (i.e. no space or
# meta characters in any keys or values), it will bail.  Fixing this
# is messy because we expect to pass the result through ssh and there
# will be multiple quote-unpackings.
env_reexport() {
    local __varname __out
    __varname="$1"
    shift

    # Bash docs suggest it's possible with [[ "foo" =~ ... ]] but this
    # is quicker to write
    __out="$( export "$@"; perl -e 'use strict; use warnings;
my @out;
foreach my $k (@ARGV) {
 die qq{Cannot treat variable name >$k< safely\n} unless
   $k =~ qr{^[a-z][a-z0-9_]*$}i;
 next unless exists $ENV{$k};
 die qq{Cannot treat variable $k contents >$ENV{$k}< safely\n} unless
   $ENV{$k} =~ qr{\A[-a-zA-Z0-9_%^()=+:@/.,]*\z};
 push @out, qq{$k=$ENV{$k}};
}
print join q{ }, @out;
'  "$@" )" ||
      bail "env_reexport: environment stringify fail"

    printf -v $__varname %s "$__out"
}

looks_like_version() {
    case "$1" in
        [0-9][0-9][0-9].[0-9][0-9] | [0-9][0-9].[0-9][0-9])
            # ok
            return 0 ;;
        *)
            return 1 ;;
    esac
}



# Related scripts assume that $( cd foo; echo bar ) chdir(./foo) and
# capture only bar .  CDPATH breaks these assumptions.  It would be
# better if the scripts had not so assumed, but this is a reliable fix
# with no external effects.
unset CDPATH

# Assumes we were called by script in this directory.  $0 is not this file!
dist_scripts="$( dirname "${BASH_SOURCE[0]}" )"

# "What we're operating upon" is now another repo
thisprog="$( _whatami )"
