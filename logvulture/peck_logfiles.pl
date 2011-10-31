#! /software/bin/perl-5.12.2 -T
#  5.12.2 just for easy Pod::Usage dep

use strict;
use warnings;
use Pod::Usage 1.36 'pod2usage'; # http://www.perlmonks.org/?node_id=551815
use YAML 'Dump';

use lib 'lib'; # assume current directory
use Log::Vulture;


=head1 NAME

peck_logfiles.pl - extract "interesting bits" from Otterlace logfiles

=head1 SYNOPSIS

Runs any of these ways,

 ./peck_logfiles.pl > out.yaml < logfiles-list.txt    # one per line

 ./peck_logfiles.pl $( cat logfiles-list.txt )  > out.yaml

 make recent.yaml

 make interesting-bits.yaml

=head1 DESCRIPTION

Operates on each of (some) logfiles in turn.  Pecks out the ZMap
stacktraces and other recognisable problem reports, keeping some
context in some cases.  Emits YAML.

=cut


sub main {
  $ENV{PATH} = '/bin:/usr/bin';
  pod2usage(-verbose => 1) if !@ARGV && -t STDIN;
  pod2usage(-verbose => 2) if "@ARGV" =~ m{(^| )(-h|--help)\b};

  my @files = (@ARGV ? @ARGV : <STDIN>);
  chomp @files;

  my %info; # key = filename, value = log output
  foreach my $fn (@files) {
    my $v = Log::Vulture->new(fname => $fn);
    if (eval { $v->fh }) {
      $info{$fn} = $v->output;
    } else {
      warn "$fn skipped: $@";
    }
  }

  warn "$0: nothing to do\n" unless @files;
  print Dump(\%info);
  return 0;
}

exit main();
