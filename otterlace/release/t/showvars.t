#! /usr/bin/env perl

use strict;
use warnings;
use Test::More tests => 10;
use File::Temp qw( tempdir );
use File::Slurp qw( write_file );
use Cwd 'abs_path';

=head1 NAME

showvars.t - sanity checks on output of _otterlace.sh

=head1 DESCRIPTION

These tests make tempdirs containing enough dist/conf/* to pretend
they are ensembl-otter, so we can ensure that builds of all ages and
platforms are represented.

It should not matter what $PWD is when this script runs.

=cut


# absolute form of the otterlace/release/ parent directory; contains scripts/ and t/
my $ottrel;

my $verbose = $ENV{HARNESS_IS_VERBOSE} || !$ENV{HARNESS_ACTIVE};

sub set_ottrel {
    # This must happen before with_temp does chdir
    $ottrel = abs_path($0);
    $ottrel =~ s{/t/showvars\.t$}{}
      or die "Cannot construct otterlace/release/ path from $ottrel";
    diag "  ottrel is $ottrel" if $verbose;
}

sub with_temp {
    my ($put_configs, $code, @code_args) = @_;

    my $tempdir = tempdir('showvars.t.XXXXXX', CLEANUP => 1, TMPDIR => 1);
    diag "  tempdir is $tempdir" if $verbose;
    chdir $tempdir or die "chdir to tmpdir: $!";
    (mkdir 'dist') && (mkdir 'dist/conf') or die "mkdir -p dist/conf: $!";

    while (my ($k, $v) = each %$put_configs) {
        write_file("dist/conf/$k", { atomic => 1 }, "$v\n");
    }

    $code->(@code_args);

    # Out, so we can delete it after.  This incidentally means that
    # failing tests leave the evidence behind.
    chdir "$tempdir/..";
}

sub showvars_tt {
    my ($name, $constpart) = @_;
    # runs chdir()d to a temp dir

    my $got = qx{ $ottrel/scripts/otterlace_showvars };

    # Take out variable parts, before comparing against template
    $got =~ s{^thisprog=(.*)$}{(TP)}m;
    my $thisprog = $1;

    is $got, $constpart, "$name: compare constant part";
    like($thisprog,
         qr{^/.*/otterlace_showvars \([0-9a-f]+ from \S+:\S+/team_tools\.git\)$},
         "$name: thisprog");
}


sub main {
    set_ottrel();
    local @ENV{qw{ otter_nfswub otter_swac SHOWVARS_TEST }}; # prevent outside influence

    my $swac = "\x2Fsoftware\x2Fanacode"; # hide from grep
    my $nfswub = "\x2Fnfs\x2FWWWdev";

    ### Release versions, pre- and post- 58
    #
    with_temp({ version_major => 56, version_minor => '02' },
              \&showvars_tt, "$swac 56.02", <<"TXT");
(TP)
full_version=56.02
wrapperfile_outside_otterdir: yes
otter install paths:
 holtdir=$swac/otter
 otter_home=$swac/otter/otter_rel56.02
 bin=$swac/otter/otter_rel56.02/bin
 wrapperfile=$swac/bin/otterlace_rel56.02
 web_lib=$nfswub/SANGER_docs/lib/otter/56
 web_cgi=$nfswub/SANGER_docs/cgi-bin/otter/56
TXT

    with_temp({ version_major => 58, version_minor => '07' },
              \&showvars_tt, "$swac 58.07", <<"TXT");
(TP)
full_version=58.07
wrapperfile_outside_otterdir: no
otter install paths:
 holtdir=$swac/otter
 otter_home=$swac/otter/otter_rel58.07
 bin=$swac/otter/otter_rel58.07/bin
 wrapperfile=$swac/otter/otter_rel58.07/bin/otterlace
 web_lib=$nfswub/SANGER_docs/lib/otter/58
 web_cgi=$nfswub/SANGER_docs/cgi-bin/otter/58
TXT


    ### Dev versions
    #
    with_temp({ version_major => 56, version_minor => '' },
              \&showvars_tt, "$swac 56 dev", <<"TXT");
(TP)
full_version=56
wrapperfile_outside_otterdir: yes
otter install paths:
 holtdir=$swac/otter
 otter_home=$swac/otter/otter_rel56
 bin=$swac/otter/otter_rel56/bin
 wrapperfile=$swac/bin/otterlace_rel56
 web_lib=$nfswub/SANGER_docs/lib/otter/56
 web_cgi=$nfswub/SANGER_docs/cgi-bin/otter/56
TXT

    my $more_tmp = tempdir('showvars.t.XXXXXX', CLEANUP => 1, TMPDIR => 1);
    diag "  more_tmp is $more_tmp" if $verbose;
    my $tmp_wub  = "$more_tmp/_httpd";
    my $tmp_swac = "$more_tmp/sw-ac";
    mkdir $tmp_wub and mkdir $tmp_swac or die "mkdir: $!";

    $ENV{otter_nfswub} = $tmp_wub;
    $ENV{otter_swac} = $tmp_swac;
    with_temp({ version_major => 58, version_minor => '' },
              \&showvars_tt, "$swac 58 dev", <<"TXT");
(TP)
full_version=58
wrapperfile_outside_otterdir: no
otter install paths:
 holtdir=$tmp_swac/otter
 otter_home=$tmp_swac/otter/otter_rel58
 bin=$tmp_swac/otter/otter_rel58/bin
 wrapperfile=$tmp_swac/otter/otter_rel58/bin/otterlace
 web_lib=$tmp_wub/lib/otter/58
 web_cgi=$tmp_wub/cgi-bin/otter/58
TXT


    # Last test - exercise other full_version combinations
    $ENV{SHOWVARS_TEST} = 1;
    with_temp({ version_major => 58, version_minor => 15 },
              \&showvars_tt, 'full_version params', <<"TXT");
(TP)
full_version=58.15
full_version(_ foopfx)=foopfx58_15
full_version(- humpub-release-)=humpub-release-58-15
abcd goldfish
full_version(. v foo) => foo=v58.15
tostdout58--dev<
oig:holtdir '' => $tmp_swac/otter<
TXT
}

main();
