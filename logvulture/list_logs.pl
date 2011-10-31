#! /software/bin/perl -T
#  ^^^^^^^^^ just a bodge for easy File::Slurp deps

use strict;
use warnings;
use File::Slurp 'read_dir';
use YAML qw( Dump LoadFile );

sub main {
  die "Syntax: $0 < -d | -a <days> >\n\n  -a\tList files up to <days> old\n  -d\tList the directories\n"
    unless @ARGV && $ARGV[0] =~ /^-([ad])/;
  my $op = shift @ARGV;
  my $erand;
  if ($op eq '-a') {
    $erand = shift @ARGV or die "Need days";
  }

  my @u = LoadFile('users.yaml');
  my %u = eval {
    %{ $u[0]->{users} };
  } or die "Bad userlist";

  my @d = sort values %u;
  my @out = @d;

  if ($erand) {
    my @logs = map { files_in(qr{/otterlace\..*\.log$}, $_) } @d;
    @out = grep { -M $_ <= $erand } @logs;
  }

  print map {"$_\n"} @out;
}


sub files_in {
  my ($pat, $dir) = @_;
  return grep { $_ =~ $pat } map {"$dir/$_"} read_dir($dir);
}

main();
