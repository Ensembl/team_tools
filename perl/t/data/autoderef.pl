#! /usr/bin/perl

use strict;
use warnings;


# This code is the test-subject for the policy.
# It doesn't need to run, but...

use Test::More tests => 8 * 7;
# these tests ensure the code is valid,
# so we're testing something real.


my $arr = [qw[ bar baz bat ]]; # OK: init
my $lad = { izzy => 'whizzy', abraca => 'dabra', piff => 'poff' }; # OK: init
my @arrlad = ($arr, $lad); # OK: init

# BAD: Plain scalar variables
{
    push $arr, "bump";
    unshift $arr, "jar";
    is('bump', pop $arr, 'weasel');
    is('jar', shift $arr, 'glug');
    is('baz', (splice $arr, 1, 1, qw( zig zag )), 'zog');
    is('abraca izzy piff', (join ' ', sort( keys $lad )), 'keys-lad');
    is('0123', (join '', keys $arr), 'keys-arr');
    is('dabra poff whizzy', (join ' ', sort( values $lad )), 'values-lad');
    is('bar zig zag bat', (join ' ', values $arr), 'values-arr');
    my @collect;
    while (my ($k, $v) = each $arr) {
        push @collect, "$k:$v";
    }
    while (my ($k, $v) = each $lad) {
        push @collect, "$k:$v";
    }
    is('0:bar 1:zig 2:zag 3:bat abraca:dabra izzy:whizzy piff:poff',
       (join ' ', sort @collect), 'eached');
}
splice @$arr, 1,2, 'baz'; # OK: reset

# BAD:v Plain scalars again with pop( ... )
{
    push($arr, "bump");
    unshift($arr, "jar");
    is('bump', pop($arr), 'weasel');
    is('jar', shift($arr), 'glug');
    is('baz', splice($arr, 1, 1, qw( zig zag )), 'zog');
    is('abraca izzy piff', (join ' ', sort keys($lad)), 'keys-lad');
    is('0123', (join '', keys($arr)), 'keys-arr');
    is('dabra poff whizzy', (join ' ', sort values($lad)), 'values-lad');
    is('bar zig zag bat', (join ' ', values($arr)), 'values-arr');
    my @collect;
    while (my ($k, $v) = each($arr)) {
        push @collect, "$k:$v";
    }
    while (my ($k, $v) = each($lad)) {
        push @collect, "$k:$v";
    }
    is('0:bar 1:zig 2:zag 3:bat abraca:dabra izzy:whizzy piff:poff',
       (join ' ', sort @collect), 'eached');
}
splice @$arr, 1,2, 'baz'; # OK: reset


# BAD: Scalars through the data structure
{
    push $arrlad[0], "bump";
    unshift $arrlad[0], "jar";
    is('bump', pop $arrlad[0], 'weasel');
    is('jar', shift $arrlad [0], 'glug'); # extra space
    is('baz', (splice $arrlad [0], 1, 1, qw( zig zag )), 'zog'); # extra space
    is('abraca izzy piff', (join ' ', sort( keys $arrlad[1] )), 'keys-lad');
    is('0123', (join '', keys $arrlad[0]), 'keys-arr');
    is('dabra poff whizzy', (join ' ', sort( values $arrlad[1] )), 'values-lad');
    is('bar zig zag bat', (join ' ', values $arrlad[0]), 'values-arr');
    my @collect;
    while (my ($k, $v) = each $arrlad[0]) {
        push @collect, "$k:$v";
    }
    while (my ($k, $v) = each $arrlad[1]) {
        push @collect, "$k:$v";
    }
    is('0:bar 1:zig 2:zag 3:bat abraca:dabra izzy:whizzy piff:poff',
       (join ' ', sort @collect), 'eached');
}
splice @$arr, 1,2, 'baz'; # OK: reset


# BAD: Scalars through the data structure, with wacky @
{
    no warnings 'syntax';
    push @arrlad[0], "bump";
    unshift @arrlad[0], "jar";
    is('bump', pop @arrlad[0], 'weasel'); # extra space
    is('jar', shift @arrlad [0], 'glug'); # extra space
    is('baz', (splice @arrlad [0], 1, 1, qw( zig zag )), 'zog');
    is('abraca izzy piff', (join ' ', sort( keys @arrlad[1] )), 'keys-lad');
    is('0123', (join '', keys @arrlad[0]), 'keys-arr');
    is('dabra poff whizzy', (join ' ', sort( values @arrlad[1] )), 'values-lad');
    is('bar zig zag bat', (join ' ', values @arrlad[0]), 'values-arr');
    my @collect;
    while (my ($k, $v) = each @arrlad[0]) {
        push @collect, "$k:$v";
    }
    while (my ($k, $v) = each @arrlad[1]) {
        push @collect, "$k:$v";
    }
    is('0:bar 1:zig 2:zag 3:bat abraca:dabra izzy:whizzy piff:poff',
       (join ' ', sort @collect), 'eached');
}
splice @$arr, 1,2, 'baz'; # OK: reset


# Again with non-variables
sub arrr { return $arr } # OK: accessor
sub ladd { return $lad } # OK: accessor


# BAD: Function calls
{
    push arrr(), "bump";
    unshift arrr(), "jar";
    is('bump', pop arrr(), 'weasel');
    is('jar', shift arrr(), 'glug');
    is('baz', (splice arrr(), 1, 1, qw( zig zag )), 'zog');
    is('abraca izzy piff', (join ' ', sort( keys ladd() )), 'keys-lad');
    is('0123', (join '', keys arrr()), 'keys-arr');
    is('dabra poff whizzy', (join ' ', sort( values ladd() )), 'values-lad');
    is('bar zig zag bat', (join ' ', values arrr()), 'values-arr');
    my @collect;
    while (my ($k, $v) = each arrr()) {
        push @collect, "$k:$v";
    }
    while (my ($k, $v) = each ladd()) {
        push @collect, "$k:$v";
    }
    is('0:bar 1:zig 2:zag 3:bat abraca:dabra izzy:whizzy piff:poff',
       (join ' ', sort @collect), 'eached');
}
splice @$arr, 1,2, 'baz'; # OK: reset

# BAD: Bareword function calls
{
    push arrr, "bump";
    unshift arrr, "jar";
    is('bump', pop arrr, 'weasel');
    is('jar', shift arrr, 'glug');
    is('baz', (splice arrr, 1, 1, qw( zig zag )), 'zog');
    is('abraca izzy piff', (join ' ', sort( keys ladd )), 'keys-lad');
    is('0123', (join '', keys arrr), 'keys-arr');
    is('dabra poff whizzy', (join ' ', sort( values ladd )), 'values-lad');
    is('bar zig zag bat', (join ' ', values arrr), 'values-arr');
    my @collect;
    while (my ($k, $v) = each arrr) {
        push @collect, "$k:$v";
    }
    while (my ($k, $v) = each ladd) {
        push @collect, "$k:$v";
    }
    is('0:bar 1:zig 2:zag 3:bat abraca:dabra izzy:whizzy piff:poff',
       (join ' ', sort @collect), 'eached');
}
splice @$arr, 1,2, 'baz'; # OK: reset


# GOOD: Some explicit deref versions, for comparison
{
    push(@$arr, "bump"); # OK: explicit
    unshift(@$arr, "jar"); # OK: explicit
    is('bump', pop(@$arr), 'weasel'); # OK: explicit
    is('jar', shift(@ $arr), 'glug'); # OK: explicit, space
    is('baz', splice(@$arr , 1, 1, qw( zig zag )), 'zog'); # OK: explicit
    is('abraca izzy piff', (join ' ', sort keys (%$lad)), 'keys-lad'); # OK: explicit, space
    is('0123', (join '', keys(@$arr)), 'keys-arr'); # OK: explicit
    is('dabra poff whizzy', (join ' ', sort values(%$lad)), 'values-lad'); # OK: explicit
    is('bar zig zag bat', (join ' ', values(@$arr)), 'values-arr'); # OK: explicit
    my @collect;
    while (my ($k, $v) = each(@$arr)) { # OK: explicit
        push @collect, "$k:$v";
    }
    while (my ($k, $v) = each(%$lad)) { # OK: explicit
        push @collect, "$k:$v";
    }
    is('0:bar 1:zig 2:zag 3:bat abraca:dabra izzy:whizzy piff:poff',
       (join ' ', sort @collect), 'eached');
}
splice @$arr, 1,2, 'baz'; # OK: reset




# Clean code which may trip up the policy --

my @elements = qw( Na K Li );
while (my $this_ele = shift @elements) { # breakage from RT#355286
    # Sing the Elements song
}

while (my $arg = shift) { # from @ARGV
    # Junk the args
}
