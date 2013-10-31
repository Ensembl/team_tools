#! /usr/bin/perl -p -i~

my @f = split /\t/;
my @a = split /\s*;\s*/, $f[8];
$f[8] = join ';', sort @a if defined $f[8];
$_ = join "\t", @f;
