#! /usr/bin/env perl
use strict;
use warnings;

use Test::More tests => 6;
use YAML qw( Dump );
use File::Slurp 'read_dir';

=head1 DESCRIPTION

Quick and dirty bodge, to let me know when F<designations.txt> does
not match the installed symlinks.

=cut

use lib '/software/anacode/otter/otter_dev/ensembl-otter/modules';
use Bio::Otter::Server::Config;

my $desig = Bio::Otter::Server::Config->designations;
my %major; # set of versions with clients, key = major

my @holtdir = map {"/software/noarch/linux-$_/anacode/otter"} qw( i386 x86_64 );
foreach my $holtdir (@holtdir) {
    # List designated versions
    my @holt_leaf = read_dir($holtdir);
    my %got_ln =
      map {( $_ => readlink($_) )}
        grep { -l $_ && $_ !~ m{/otter_production_main} }
          map {"$holtdir/$_"} @holt_leaf;

    # List other non-designated version
    my %got_nondes; # key = major, val = latest major.minor
    foreach my $vsn (sort grep { ! -l "$holtdir/$_" && -d _ } @holt_leaf) {
        # "latest" minor version is given by loop's sort
        my ($maj, $min) =
          $vsn =~ m{_rel(\d+)(?:\.(\d+))?$} or
            die "Incomprehensible otter client $holtdir/$vsn";
        $got_nondes{$maj} = defined $min ? "$maj.$min" : 'dev';
        $major{$maj} = 1;
    }

    # Build expected designation list
    my %want_ln = map {( "$holtdir/otter_$_" => "otter_rel$$desig{$_}" )}
      grep { ! /^\d+$/ } keys %$desig;

    # Build expected non-designated list
    while (my ($k, $v) = each %got_ln) {
        my ($maj, $min) =
          $v =~ m{_rel(\d+)(?:\.(\d+))?$} or
            die "Incomprehensible otter client $k => $v";
        delete $got_nondes{$maj};
    }
    my %want_nondes = map {( $_ => $desig->{$_} )}
      grep { /^\d+$/ } keys %$desig;

    my $pass = 1;
    $pass = 0 unless is_deeply(\%got_ln, \%want_ln, "Symlinks in $holtdir");
    $pass = 0 unless is_deeply(\%got_nondes, \%want_nondes, "Non-designated in $holtdir");
    diag Dump({ desig => $desig,
                got_non_designated => \%got_nondes,
                want_non_designated => \%want_nondes,
                got_ln => \%got_ln, want_ln => \%want_ln })
      unless $pass;
}

foreach my $dir (map {"/nfs/WWWdev/SANGER_docs/$_/otter"} qw( lib cgi-bin )) {
    my @got  = sort grep { -d "$dir/$_" } read_dir($dir);
    my @want = sort keys %major;
    is_deeply(\@got, \@want, "Major versions in $dir (dev)");
}
