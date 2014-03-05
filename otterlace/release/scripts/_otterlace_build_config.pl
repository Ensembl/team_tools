#! /usr/bin/env perl

use strict;
use warnings;
use YAML 'LoadFile';
use FindBin '$RealBin';


=head1 NAME

_otterlace_build_config.pl - calculate config for otterlace_build

=head1 SYNOPSIS

 _otterlace_build_config [ options ] <ZMap_build_tree>*

=head1 DESCRIPTION

Otterlace builds require a zmap build directory, a build host, maybe
some other configuration to set the destination, and the sources for
otterlace.

This script outputs shell-useful build spec.

=head2 Rationale

It has for various reasons not been possible to calculate the build
directory from anything.  This script therefore requires that as
input.

It has also sometimes not been possible to calculate the otterlace
build host from the arch & distro; the full set of
C<< {lenny,lucid}-dev{32,64} >> are not always available.


=head2 Operations

For a list of build directories and some (hopefully stable) data, we
can produce either

=over 4

=item (with no option flags)

Output the set of Otterlace build hosts, for the given ZMap build
tree(s).

=item -host <otterlace_buildhost>

Output the one ZMap build tree (chosen from the set of inputs) for a
particular Otterlace build host.

This program is called to reproduce the main calculation multiple
times, in order to answer these queries; but that is easier than
passing structured data back to the calling shell, or taking control
of the build as another wrapper on the outside.

=item -list

Output the list of build host combinations.  Used for inclusion in
help text.

=back

=cut

our %CONFIG; # populated in read_config

sub main {
    # What to do?
    my ($op, $for_host);
    if (@ARGV > 2 && $ARGV[0] eq '-host') {
        ($op, $for_host) = splice @ARGV, 0, 2;
    } elsif (@ARGV == 1 && $ARGV[0] eq '-list') {
        $op = shift @ARGV;
    }
    my @zmapdirs = @ARGV;

    # Sane?
    if (!@zmapdirs && !$op) {
        die "Syntax: $0 [ -host <otterlace_buildhost> ] <ZMap_build_tree>*
\t$0 -list\n
  Without -host flag, output the set of Otterlace build hosts for
  these ZMap trees.\n
  When given a buildhost (which must be from that set), return the one
  ZMap tree which should be used on the host.\n";
    }
    my @missing_dir = grep { ! -d $_ }
      map {($_, "$_/ZMap", "$_/Dist")} # should exist in valid build; mca inferred
        @zmapdirs;
    die "$0: ZMap build directories (@zmapdirs) are missing some component directories (@missing_dir)\n"
      if @missing_dir;

    # Read config
    my $cfg_fn = "$RealBin/../build-config.yaml"; # TODO: a flag
    eval { read_config($cfg_fn) } or
      die "$0: Failed to read $cfg_fn: $@";

    # Mappings: find the ZMap build hosts
    my %zhost2dir = zhosts(@zmapdirs);

    # Mappings: find the ones we want, then whinge about new ones
    my %ohost2dir;
    while (my ($zhost, $dir) = each %zhost2dir) {
        my $ohost_list = $CONFIG{zhost2ohost}{$zhost};
        foreach my $ohost (split / /, ($ohost_list || '')) {
            $ohost2dir{$ohost} = $dir;
        }
    }
    my @unk_zhost = sort # returned by zhosts, but not in our config
      grep { !exists $CONFIG{zhost2ohost}{$_} } keys %zhost2dir;
    die "$0: unknown (new?) zmap build hosts\n".
      "  Please tell me about: @unk_zhost\nby adding to $cfg_fn" if @unk_zhost;

    # Output
    my @ohost = sort keys %ohost2dir;
    if (!defined $op) {
        print join '', map {"$_\n"} @ohost;
    } elsif ($op eq '-list') {
        print " Current build hosts are configured in\n $cfg_fn\n\n";
        print "    ZMap build host     Otterlace build host\n";
        my $z2o = $CONFIG{zhost2ohost};
        print map {
            my $ohost = $$z2o{$_};
            sprintf("      %-16s    %s\n", $_, defined $ohost ? $ohost : '(no build)');
        } sort keys %$z2o;
    } elsif ($op eq '-host') {
        my $dir = $ohost2dir{$for_host};
        die "$0: $for_host is not an Otterlace build host,\n".
          "  Given zmapdirs (@zmapdirs)\n".
          "  Valid hosts are (@ohost)\n" unless defined $dir;
        printf("%s\n", $dir);
    } else {
        die "$0: Confused. op=$op";
    }

    return 0;
}

# Return list of build hosts, extracted from the ZMap tree's symlinks
sub zhosts {
    my @dir = @_;
    my %h2d; # output

    foreach my $dir (sort @dir) {
        # sort makes it stable, but not always chronological.
        # order should not matter

        # Assumption: the build for each arch has one symlink pointing to it
        opendir my $dh, $dir or die "Scan $dir: $!";
        my @link = grep { -l $_ } map {"$dir/$_"} readdir $dh;
        die "No symlinks in $dir - all builds failed?" unless @link;

        # Assumption: each symlink points to ZMap.$HOST/$BLAH
        foreach my $ln (@link) {
            my $dest = readlink $ln;
            die "$0: readlink $ln: $!" unless defined $dest;
            die "$0: $ln -> $dest: cannot comprehend this" unless
              $dest =~ m{(?:^|/)ZMap\.([-a-zA-Z0-9.]+)(/|$)};
            my $host = $1;
            # Alternations in regexp are an attempt to second-guess changes

            $h2d{$host} = $dir;
        }
    }
    return %h2d;
}

sub read_config {
    my ($fn) = @_;
    my @cfg = LoadFile($fn);
    die "Expected one hash" unless eval { $cfg[0]{zhost2ohost} };
    %CONFIG = %{ shift @cfg };
    return 1;
}


exit main();
