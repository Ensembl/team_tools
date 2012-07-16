#! /usr/bin/env perl

use strict;
use warnings;
use YAML::XS 'Dump';

=head1 NAME

vsn_merge.pl - reconstruct a "live" branch

=head1 DESCRIPTION

Constructs from scratch a branch consisting of merges from each
Otter-version branch.

=head2 Input

A set of split-out branches, one per Otter version and another for
meta-data files.  This is made by F<vsn_split.sh> .

=head2 Expected usage

This is a transient script.

Once it works, periodically to maintain a live branch.

When fresh commits come in on version branches and are merged to live
by the author, the script won't be needed.

=cut

our $DEBUG = 0;

sub main {
    $|=1;

    # List branches, sanity check repo
    my @vsn = sort map { /^\*?\s*(\d+|meta)$/ ? ($1) : () } qx( git branch );
    die "Expected Otter Data version branches from 52, got (@vsn)"
      unless "@vsn" =~ /^52 53 .* meta$/;
    die "Must be at top of clone" unless -d '.git';

    run(is_clean => qw( git diff --quiet --exit-code --cached ));
    run(is_clean => qw( git diff --quiet --exit-code ));

    my $IEC = '96971e864e41878425906ee80ac04880db967176';

    run(jump => qw( git checkout meta ));
    run("fresh branch" => qw( git branch -f live_new ), $IEC);
    # don't tromp branch named "live"!

    my @ci; # list of [ unixtime, ciid, comment, branchname ], with ascending time
    my %branch_date_ciid; # key = $branch, value = \@ci for that branch
    foreach my $br (@vsn) {
        my @ts = do {
            local $/="\x00";
            qx( git log --pretty=tformat:"%at%x09%H%x09%B%x00" $br -- );
        };

        # chompage of separator and comment
        foreach (@ts) { s{^\n+}{}; s{\n*\x00$}{} }

        my @br_ci =
          (reverse
           map {[ (split /\t/, $_), $br ]}
           grep { $_ ne '' } @ts);
        $branch_date_ciid{$br} = \@br_ci;

        # Every branch should start from IEC
        my $br_iec = (shift @br_ci)->[1];
        die "Branch $br has bad IEC ($br_iec != $IEC)" unless $br_iec eq $IEC;
    }
    # copy to flat list
    @ci = (sort { $a->[0] <=> $b->[0] || $a->[-1] cmp $b->[-1] } # by unixtime or branchname
           map { @$_ }
           values %branch_date_ciid);

    # build a set of merges
    run(checkout => qw( git checkout live_new ));
    run("configure to copy" => qw( git config tar.copy.command ), 'tar xf -');
    my $until; # unixtimes of current merge
    my %since; # key=branch, value=unixtime of last merge from that branch
    my %merge_ci; # key = $branch, value = $ci[n]
    foreach my $ci (@ci) {
        my ($utime, $ciid, $comment, $branch) = @$ci;
        my @hit = $comment =~ m{^autocommit \((meta|\d+) :synced:(all|\g1)\)};
        next unless @hit;

        # do we octopus up some more branches, or start a new merge commit?
        if (defined $until && $until ne $utime) {
            make_merge($until, values %merge_ci);
            $since{keys %merge_ci} = ($until) x keys %merge_ci;
            %merge_ci = ();
        }
        $until = $utime;

        foreach my $br ($branch eq 'meta' ? @vsn : ($branch)) {
            $merge_ci{$br} ||=
              find_latest($since{$br}, $until, $branch_date_ciid{$br});
        }
    }
    make_merge($until, values %merge_ci) if %merge_ci;

#    print Dump({ zbdc => \%branch_date_ciid, ci => \@ci });

    return 0;
}

sub run {
    my ($what, @cmd) = @_;
    system(@cmd);
    die "run: '$what' failed ".
      ($? == -1 ? $! : sprintf("0x%X", $?)).
        "\n  (@cmd)\n"
          if $?;
    print "ran $what => (@cmd)\n" if $DEBUG;
}

sub find_latest {
    my ($since, $until, $ci) = @_;
    my $top = $ci->[0];

    $ci = [ @$ci ]; # copy, do O(N^2) searching; otherwise be faster and destructive

    $since ||= 0;
    my %ci; # noteworthy commits (for debug & state)

    while (@$ci > 0 && $ci->[0][0] <= $since) { $ci{presince} = shift @$ci }
    while (@$ci > 1 && $ci->[1][0] <= $until) { $ci{preuntil} = shift @$ci }

    if (@$ci && $ci->[0][0] <= $until) {
        $ci{return} = $ci->[0];
    }

    if ($DEBUG) {
        printf "  ? Since %s\tuntil %s\ton %s\n",
          iso8601($since), iso8601($until), $top->[3];
        foreach my $n (qw( presince preuntil return )) {
            printf "  %s %s %s\n", ($n eq 'return' ? '>' : ' '),
              $n, show_ci($ci{$n});
        }
    }

    return $ci{return} || ();
}

sub make_merge {
    my ($utime, @ci) = @_;
    @ci = grep { defined } @ci;

    # For simple & interactive use,
    #   git merge --no-ff -s recursive -X $branch
    # would do it; except for the first commit on the branch, which has to
    # be done with --no-commit and a 'git mv' to the subdirectory
    #
    # When reconstructing history I want octopus-like merges and maybe
    # reconstruct the canonical users.txt & species.dat

    open my $MH, '>', '.git/MERGE_HEAD' or die "Can't write MERGE_HEAD: $!";
    foreach my $ci (sort { $a->[3] cmp $b->[3] } @ci) {
        my ($time, $ciid, $comment, $br) = @$ci;
        print {$MH} "$ciid\n" or die "print{MH}: $!";
        next if $br eq 'meta'; # can include meta among merge parents, but not its content

        run(replace => qw( git rm -rfq ), "$br/") if -d $br;
        my @new_fn = qx( git ls-tree $ciid );
        if (@new_fn) {
            run(replace => qw( git archive --format=copy ), "--prefix=$br/", $ciid);
            # piped into tar.copy.command, outputs (nothing) to our stdout
            run(add => qw( git add -A ), "$br/");
        } else {
            print "Branch $br became empty\n"; # git-archive(1) cannot cope
        }
    }
    close $MH or die "close{MH}: $!";

    open my $MM, '>', '.git/MERGE_MODE' or die "Can't write MERGE_MODE: $!";
    print {$MM} 'no-ff' or die "print{MM}: $!"; # \n breaks it
    close $MM or die "close {MM}: $!";

    if (system(qw( git diff --quiet --exit-code --cached ))) {
        local @ENV{qw{ GIT_AUTHOR_DATE GIT_COMMITTER_DATE }} = ("$utime +0000") x 2;
        local $ENV{GIT_COMMITTER_NAME} = 'Reconstruction';
        run(commit => qw( git commit -m ), qq{octo!\n\nreconstruction of webpublished content from dev branches + diffdevlive\n  may contain false positives & false negatives});
    } else {
        warn "no diff, skip commit\n";
        unlink qw( .git/MERGE_HEAD .git/MERGE_MODE );
    }

    if ($DEBUG) {
        print " ...:";
        my $foo = <STDIN>;
    }
}

sub show_ci {
    my ($ci) = @_;
    return 'nil' unless $ci;
    my ($t, $id, $comm, $br) = @$ci;
    return sprintf("%s(%s):%s", $id, $br, iso8601($t));
}

sub iso8601 {
    my ($utime) = @_;
    my @t = localtime($utime);
    return sprintf("%04d-%02d-%02d %02d:%02d:%02d", $t[5]+1900, $t[4]+1, @t[3,2,1,0]);
}

exit main();
