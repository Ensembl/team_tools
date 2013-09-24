#! /usr/bin/perl

use strict;
use warnings;
use Test::More;
use YAML 'Dump';
use Perl::Critic;


sub main {
    my $fn = $0;
    $fn =~ s{\w+\.t$}{data/autoderef.pl} or die "Can't find data from $0";
    my $KW_RE = qr{push|unshift|pop|shift|splice|keys|values|each};
    plan tests => 4;

    # Find the expected bad
    my @bad; # list of [ $row, $col, $keyword, $value, $whole_line ]
    open my $fh, '<', $fn or die "Can't read subject code $fn: $!";
    while (<$fh>) {
        next if /^\s*#/;
        next if /# OK:/;
        next unless m{arr|lad};

        my ($pfx, $kw, $val) =
          m{^(.*?)                              # ...stuff
            ($KW_RE(?:\s*\()?)\s*               # built-in(
            ([\@\$]?(?:arr|lad)\w*(?:\(\)|\\[\d]|)) # list-or-hash-ref
       }x or die "Misparsed 'bad' line $_";
        push @bad, [ $., 1+length $pfx, "$kw $val", $_ ];
    }
    is(scalar @bad, 11 * 6, 'expected bad count')
      or diag Dump({ bad => \@bad });


    # Get the critic
    my $critic = Perl::Critic->new
      (-profile => '', # don't use a profile
       '-single-policy' => 'ProhibitAutoderef');

    my @policy = $critic->policies;
    is(1, scalar @policy, 'got policy')
      or diag Dump({ policy => \@policy });

    # Check the violations
    my $nV = my @violations = $critic->critique($fn);
    is(scalar @violations, scalar @bad, 'violation count')
      or diag  "@violations\n$nV violations\n";

    my @got_vio = map {
        [ $_->line_number,
          $_->column_number,
          $_->to_string,
          $_->source ];
    } @violations;

    my @cmp_vio = zip_vio(\@got_vio, \@bad);
    my @mismatch = grep { !$_->[0] || !$_->[1] } @cmp_vio;
    is(0, scalar @mismatch, 'no mismatched violations')
      or diag Dump({ mismatch => \@mismatch });

    return;
}


sub zip_vio {
    my ($got_in, $want_in) = @_;
    my @out;

    # Copy for destructive ops
    my @got = @$got_in;
    my @want = @$want_in;

    while (@got || @want) {
        my $w = shift @want;
        my $g = shift @got;

        if ($w && $g) {
            # Same?
            if ($g->[0] == $w->[0] &&
                $g->[1] == $w->[1]) {
                push @out, [ $w, $g ];
            } else {
                push @out, [ undef, $g ], [ $w, undef ];
            }
        } else {
            # Stragglers
            push @out, [ undef, $g ] if $g;
            push @out, [ $w, undef ] if $w;
        }
    }

    return @out;
}


main();
