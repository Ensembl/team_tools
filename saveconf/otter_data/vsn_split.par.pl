#! /usr/bin/env perl

use strict;
use warnings;

sub main {
    my ($branch, $iec) = @ARGV; # from vsn_split.sh
    my $old = <STDIN>; # from git-filter-branch

    if ($old =~ /\S/) {
        # there are parents, make no changes
        print $old;
    } elsif ($branch =~ /^\d+$/) {
        # no parents - find the previous branch
        #
        # by doing this, we may elide a commit which would otherwise
        # have indicated that this branch was promptly webpublished
        # (but subtracting a second in latest_match helps)
        my $ci = latest_match($ENV{GIT_AUTHOR_DATE}, $branch - 1) || $iec;
        print "-p $ci\n";
    } elsif ($ENV{GIT_COMMIT} eq $iec) {
        # at the bottom, leave it alone
        print $old;
    } else {
        # meta & root start at the bottom
        print "-p $iec\n";
    }
    return 0;
}

sub latest_match {
    my ($latest, $src_branch) = @_;
    ($latest, my $tz) = $latest =~ m{^(\d+)(.*)$};
    $latest --;
    $latest .= $tz;
    my @ci = qx( git log -1 --until='$latest' --format=%H $src_branch );
#    warn "\nsearch --until='$latest' $src_branch : @ci\n";
    warn "\ngit log failed ($?) => no parent for this version branch\n" if $?;
    return @ci ? $ci[0] : ();
}

exit main();
