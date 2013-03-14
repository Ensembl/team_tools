#! /usr/bin/env perl
use strict;
use warnings;

use Test::More tests => 2;
use YAML qw( Dump );
use File::Slurp 'read_dir';

=head1 DESCRIPTION

Quick and dirty bodge, to let me know when F<designations.txt> does
not match the installed symlinks.

=cut

use lib '/software/anacode/otter/otter_dev/ensembl-otter/modules';
use Bio::Otter::Server::Config;

my $desig = Bio::Otter::Server::Config->designations;

my @holtdir = map {"/software/noarch/linux-$_/anacode/otter"} qw( i386 x86_64 );
foreach my $holtdir (@holtdir) {
    my %got_ln =
      map {( $_ => readlink($_) )}
        grep { -l $_ && $_ !~ m{/otter_production_main} }
          map {"$holtdir/$_"} read_dir($holtdir);

    my %want_ln = map {( "$holtdir/otter_$_" => "otter_rel$$desig{$_}" )}
      keys %$desig;

    is_deeply(\%got_ln, \%want_ln, "Symlinks in $holtdir")
      or diag Dump({ desig => $desig, got_ln => \%got_ln, want_ln => \%want_ln });
}
