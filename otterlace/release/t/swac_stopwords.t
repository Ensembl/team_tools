#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;
use YAML 'Dump';

=head1 NAME

t/swac_stopwords.t - keep hardwired internal pathname at bay

=head1 DESCRIPTION

Search in this repository...  and later, in ensembl-otter too?  for
Sanger-internal path names.

=head2 Obfuscation

To make this sort of test work, it becomes necessary to obfuscate the
target strings in some places.

This can then make it much harder to find them again, if that should
become necessary later.  (Compare the \x6Efs mess caused by webpublish
rejecting references to the Network File System.)

Therefore, I suggest adopting a standard substitution C<$swac>, to be
set to the expansion of C</softw*re/anac*de>.  We can then at least
search for C<< qr{\bswac\b} >>.

=cut


plan tests => 4;

my $verbose = $ENV{HARNESS_IS_VERBOSE} || !$ENV{HARNESS_ACTIVE};
my $ptn = q{'/software/|/noarch/|/anacode/'}; # @whitelist

sub main {
    my $where = $0;
    $where =~ s{t/swac_stopwords\.t$}{}
      or die "Cannot make search directory from $where";
    $where = '.' if $where eq '';
    diag "in $where" if $verbose;

    #my @hits = qx( git grep -nE $ptn );
    my @hits = qx( grep -rnE $ptn $where );
    is($?, 0, 'grep exit code'); # zero because it finds the line above, at least
    cmp_ok(scalar @hits, '>', 0, "expect one hit for this file");

    my @whitelist =
      (qr{^\S+/swac_stopwords\.t:.*ptn = .*whitelist$},
       qr{^\S+/scripts/_otterlace.sh:\d+:\s*swac=\S+$},
       qr{^\S+/scripts/otterlace_build:\d+:\s*build_log=/nfs/anac.de/otterlace/},
       qr{^\S+/scripts/otterlace_release_tag:.*LockDir=.*dummies/anac.de/CVSROOT/config},
       qr{^\S+/scripts/otterlace_release_tag:.*:/repos/git/anac.de/ensembl-otter},
      );

    # Remove whitelisted lines exactly once each
    my %wh_fail;
    while (my $wh = shift @whitelist) {
        my @n = grep { $hits[$_] =~ $wh } (0 .. $#hits);
        if (1 == @n) {
            my ($h) = splice @hits, $n[0], 1;
            chomp $h;
            diag "whitelisted '$h' =~ $wh" if $verbose;
        } else {
            $wh_fail{"$wh"} = @n." hits, not used";
        }
    }
    is(scalar keys %wh_fail, 0, 'whitelist failure')
      or diag(Dump({ wh_fail => \%wh_fail }));

    # Expect nothing else
    is(scalar @hits, 0, 'stopword hits')
      or diag(join '', "\n", map {"      $_"} @hits);
}

main();
