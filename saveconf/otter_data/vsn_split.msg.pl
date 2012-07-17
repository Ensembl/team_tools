#! /usr/bin/env perl
use strict;
use warnings;
use List::MoreUtils 'uniq';


sub get_diff {
  my ($ciid, $vsn) = @_;

  # If we didn't have diffdevlive output for the tree, we know nothing about sync
  my $ddl_treeln = qx( git ls-tree -l $ciid meta/ddl.asc ); # diffdevlive directory summary
  my ($ddlsha, $ddllen) = $ddl_treeln =~ m{^100644\s+blob\s+(\S{40})\s+(\d+)\s+};
  return '' unless $ddllen && $ddllen > 50; # no useful info

  # If we got per-file diff info, we find out what was in sync
  # ...probably.  diffdevlive has been unreliable (empty output) in
  # this period!
  my $diff_treeln = qx( git ls-tree -l $ciid meta/details.diff ); # something like a diff
  if (my ($diffsha, $difflen) = $diff_treeln =~
      m{^100644\s+blob\s+(\S{40})\s+(\d+)\s+}) {
    if ($difflen == 0) {
      return ' :synced:all'; # published, across all versions
    } else {
      my @difftxt = qx( git cat-file blob $diffsha );
      my @diff_vsn = uniq sort map { m{^--- /nfs/WWWdev/SANGER_docs/data/otter(/\d*)/(?:[^/]+)$} ? ($1) : () } @difftxt;
      @diff_vsn = map { m{^/?(\d+)$} ? $1 : 'root' } @diff_vsn;
      if ($vsn =~ /^(\d+|root)$/) {
	return ' -unsync-'     if grep { $_ eq $vsn } @diff_vsn; # diff on our version
	return " :synced:$vsn" if grep { m{^\d+$}   } @diff_vsn; # diff on another version
      }
      return " -unsync-@diff_vsn-" if @diff_vsn; # tell all unsynced, if we're making $vsn='meta' branch
      return ' ?unsync?'; # we lack recognisable diff text
    }
  } else {
    return ''; # no useful info
  }
}


sub main {
    my $vsn = shift @ARGV; # name of nascent branch = Otter version, from vsn_split.sh

    $_ = do { local $/; <STDIN> }; # commit message text from git-filter-branch

    my $pubclean = get_diff($ENV{GIT_COMMIT}, $vsn);


    if (s{\Aupdated by /nfs/users/nfs_m/mca/gitwk-(?:-bg/team_tools|anacode/team_tools(?:\.stable)?)/saveconf/update\.sh, fetch took (\d+) sec\n*\n}
         {autocommit ($vsn$pubclean)\n\n}s) {
        # warn "\nslow=$1\n" if $1 > 130; # think I logged this here to keep an eye on it, and so know that timestamps are vague

        my @changed = qx( git log -n 1 --format=%H --name-only $ENV{GIT_COMMIT} );
        splice @changed, 0, 2; # %H\n\n
        @changed = sort map {( $vsn eq 'root' ? m{^([^/]+)\n$} : m{^$vsn/(.*)\n$})
                               ? ($1) : () } @changed;
        @changed = grep { ! m{^\.#|(~|#|\.bak)$} } @changed;

        s{^(autocommit.*)}{$1: @changed} if @changed;
    }

    print;
}

main();
