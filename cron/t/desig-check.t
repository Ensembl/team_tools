#! /usr/bin/env perl
use strict;
use warnings;

use Test::More tests => 10;
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
diag "Taking BOSConfig from ".Bio::Otter::Server::Config->data_dir;

my @holtdir = map {"/software/noarch/$_/anacode/otter"}
  qw( linux-i386 linux-x86_64 precise-x86_64 ); # XXX: This trick will stop working after Lenny+Lucid
foreach my $holtdir (@holtdir) {
    diag "In $holtdir";

    # List designated versions
    my @holt_leaf = read_dir($holtdir);
    my %got_ln =
      map {( $_ => readlink($_) )}
        grep { -l $_ && $_ !~ m{/otter_production_main} }
          map {"$holtdir/$_"} @holt_leaf;

    my @dev_feat; # notice feature-branch dev builds so we can subtract their designating symlinks

    # List other non-designated version
    my %got_nondes; # key = major, val = latest major.minor
    foreach my $vsn (sort grep { ! -l "$holtdir/$_" && -d _ } @holt_leaf) {
        # "latest" minor version is given by loop's sort
        my ($maj, $min, $feat) =
          $vsn =~ m{_rel(\d+)(?:\.(\d+))?(?:_(\w+))?$} or
            die "Incomprehensible otter client $holtdir/$vsn";
        if (defined $feat) {
            $min = '' unless defined $min;
            push @dev_feat, "${maj}${min}_$feat";
        } else {
            $got_nondes{$maj} = defined $min ? "$maj.$min" : $maj;
            $major{$maj} = 1;
        }
    }

    # Build expected designation list
    my %want_ln = map {( "$holtdir/otter_$_" => "otter_rel$$desig{$_}" )}
      grep { ! /^\d+$/ } keys %$desig;

    # Build expected non-designated list
    while (my ($k, $v) = each %got_ln) {
        my ($maj, $min, $feat) =
          $v =~ m{_rel(\d+)(?:\.(\d+)|_(\w+))?$} or
            die "Incomprehensible otter client $k => $v";
        if (defined $feat) {
            my @found_feat = grep { "${maj}_$feat" eq $_ } @dev_feat;
            if (@found_feat) {
                # valid feature branch designation - won't be part of
                # %want_ln because designations.txt doesn't include
                # them
                delete $got_ln{$k};
            } else {
                # didn't see the directory, provoke a failure
                $got_ln{$k} = "$v (feature branch dangles)";
            }
        }
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

    diag "Saw feature branches: @dev_feat\n" if @dev_feat;
}

my @dir = map {("/nfs/anacode/WEBVM_docs.live/$_/otter",
                "/nfs/anacode/WEBVM_docs.dev/$_/otter" )} qw( lib cgi-bin );
foreach my $dir (@dir) {
    my @got  = sort grep { -d "$dir/$_" } read_dir($dir);
    my @want = sort keys %major;
    is_deeply(\@got, \@want, "Major versions in $dir")
      or diag explain { got => \@got, want => \@want, in => $dir };
}
