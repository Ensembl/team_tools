#! /usr/bin/env perl

use strict;
use warnings;
use Test::More tests => 4;
use Cwd 'abs_path';


=head1 NAME

t/build_log_entry.t - constrain otterlace_build_log_entry

=cut



# absolute form of the otterlace/release/ parent directory; contains scripts/ and t/
my $ottrel;

my $verbose = $ENV{HARNESS_IS_VERBOSE} || !$ENV{HARNESS_ACTIVE};

sub set_ottrel {
    # This must happen before with_temp does chdir
    $ottrel = abs_path($0);
    $ottrel =~ s{/t/build_log_entry\.t$}{}
      or die "Cannot construct otterlace/release/ path from $ottrel";
    diag "  ottrel is $ottrel" if $verbose;
}
### XXX:DUP above is from t/showvars.t


sub main {
    set_ottrel();
    syscmp_tt("realistic junk",
              [ "$ottrel/scripts/otterlace_build_log_entry",
                '--date' => 'foo bar',
                '--version' => 12.34,
                '--commit' => '0123456789012345678901234567890123456789',
                '--zmap' => '/nfs/blah/de/blah',
                '--seqtools' => 'SeqTools - ver.sion' ], <<'YAML');
---
commit: 0123456789012345678901234567890123456789
date: foo bar
seqtools: 'SeqTools - ver.sion'
version: 12.34
zmap: /nfs/blah/de/blah
YAML

    syscmp_tt("arbitrary junk",
              [ "$ottrel/scripts/otterlace_build_log_entry",
                qw[ -a b --c d e f ]], <<'YAML');
---
a: b
c: d
e: f
YAML

}

sub syscmp_tt {
    my ($name, $cmd, $expect) = @_;
    open my $fh, '-|', @$cmd
      or die "Command (@$cmd) fork failed: $!";
    my $got = do { local $/; <$fh> };
    close $fh;
    is($?, 0, "$name: exit code");
    is($got, $expect, "$name: output"); # possibly fragile, might need to YAML::Load and is_deeply
}

main();
