#! /usr/bin/env perl
use strict;
use warnings;
use YAML 'Dump';
use Time::Local 'timelocal';

sub main {
    my %f; # key = logname, value = { crash => \@time, finish => \@time }

    while (<>) {
        my ($log) = m{^([^: ]+log):\d+:} or die "Logname? in $_";
        my ($t, $what) = m{log:\d+:(\d{4}-\d{2}-\d{2} [0-9:,]+) otter\.children WARN: (Dotter finished|X Error)}
          or next;
        push @{ $f{$log}{$what} }, time_frac($t);
    }

    while (my ($log, $inf) = each %f) {
        my ($crash) = @{ $inf->{'X Error'} };
        my @dotfin = @{ $inf->{'Dotter finished'} || [] };
        if (@dotfin) {
            my @diff = map { $_ - $crash } @dotfin;
            my @abs = sort { abs($a) <=> abs($b) } @diff;
            $f{$log} = $abs[0]; # "$abs[0] : @diff";
        } else {
#            warn "No dotters seen for $log\n";
            $f{$log} = 'no_dotter';
        }
    }

    print Dump(\%f);
    my @diff = do {
        no warnings;
        sort { $a <=> $b or $a cmp $b } values %f;
    };
    print map {"$_\n"} @diff;
}

#/nfs/users/nfs_a/al1/.otter/.snapshot/weekly.10/otterlace.21900-a.log:36477:2014-03-20 10:45:24,5780 otter.children WARN: Dotter finished
#/nfs/users/nfs_a/al1/.otter/.snapshot/weekly.10/otterlace.21900-a.log:38536:2014-03-20 10:53:20,8057 otter.children WARN: X Error of failed request:  BadWindow (invalid Window parameter)

sub time_frac {
    my ($t_str) = @_;
    my ($Y,$M,$D, $h,$m,$s, $fr) = $t_str =~ m{(\d{4})-(\d{2})-(\d{2}) (\d\d):(\d\d):(\d\d),(\d{4})$}
      or die "bad time $t_str";

    return timelocal($s,$m,$h,$D,$M-1,$Y-1900) + $fr/1E4;
}

main();
