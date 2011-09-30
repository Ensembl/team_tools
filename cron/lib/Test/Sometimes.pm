package Test::Sometimes;

use strict;
use warnings;

use File::Temp 'tempfile';


=head1 NAME

Test::Sometimes - reduce a recurring test's run frequency via
filesystem stamps

=head1 SYNOPSIS

 use Test::More; # do not plan here
 use Test::Sometimes 'skip_sometimes';
 
 # want to run once every 8 hours, skip every other time
 skip_sometimes(8 * 3600);
 plan tests => 5;

=head1 DESCRIPTION

This uses dotfiles adjacent to the test script to control run
frequency, and is driven by C<$0>.

=head2 What it doesn't do

Deal with chdir calls between program start and calling
skip_sometimes.  So don't do that.

There is no "central" registry to prevent multiple copies of the
script running more frequently, or simultaneously.

There is no locking during stamp update.  This might be useful to
prevent parallel running of the same copy, but other tools exist to do
this already.

It doesn't repeat the test any more frequently after failures.  The
intention is to cut down on needless machine poundings and on noise
emitted for ongoing failures.

Account for C<time() - $^T>.  We assume the process starts, runs and
exits like a normal test under L<prove(1)>.

It doesn't schedule test runs, nor give any indication how long to
wait in order to match the run-interval closely.


=head1 EXPORTABLE SUBROUTINE

=cut

use base 'Exporter';
our @EXPORT_OK = qw( skip_sometimes );


=head2 skip_sometimes($interval_seconds)

If the test has run recently, within the interval, print a TAP skip
message and call C<exit(0)>.

Otherwise, create or update the timestamp file and return nothing.

If the timestamp file cannot be touched, generate an error.

=cut

sub skip_sometimes {
    my ($run_every_seconds) = @_;

    my $fn = stamp_filename();
    if (-f $fn) {
        my $age_sec = 86400 * -M _;
        if ($age_sec < $run_every_seconds) {
            # Stamp exists and is newer than our run interval.
            my $age = time_in_units($age_sec);
            my $interval = time_in_units($run_every_seconds);
            do_skip("Ran this $age ago, plan to run every $interval");
            # not reached
        } else {
            # Update the stamp and proceed with the run.
            utime(undef, undef, $fn)
              or die "Failed to touch $fn: $!";
        }
    } else {
        # Create the stamp
        my ($fh, $new_fn) = tempfile("$fn.XXXXXX");
        chmod (0777 &~ umask(), $new_fn); # prefer it to be public, just to fit with other files
        close $fh;
        rename($new_fn, $fn) or die "Failed rename to $fn: $!";
    }

    return (); # may already have called exit
}

sub stamp_filename {
    my $stampfile = $0;
    $stampfile =~ s{^(.*?)([^/]+)$}{$1.$2.test-sometimes}
      or die "Failed to construct stampfile from $stampfile";
    return $stampfile;
}

sub time_in_units {
    my ($sec) = @_;
    my @out;
    foreach my $pair ([ week => 604800 ], [ day => 86400 ],
                      [ h => 3600 ], [ m => 60 ], [ s => 1 ]) {
        my ($unit_name, $unit_size) = @$pair;
        if ($sec > $unit_size) {
            my $n = int($sec / $unit_size);
            push @out,
              (length($unit_name) == 1
               ? sprintf("%d%s", $n, $unit_name)
               : sprintf("%d %s%s", $n, $unit_name, $n == 1 ? '' : 's'));
            $sec -= $n * $unit_size;
        }
    }
    @out = "$sec sec" unless @out;
    return join ' ', "@out";
}

sub do_skip {
    my ($msg) = @_;

    if ($INC{"Test/More.pm"}) {
        # use the provided mechanism, else it will decide we failed
        Test::More::plan skip_all => $msg;
        # should call exit for us
    } else {
        print "1..0 # Skipped: $msg\n";
    }
    exit(0);
}

1;
