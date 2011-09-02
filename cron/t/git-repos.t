#! /usr/bin/perl -T
use strict;
use warnings;

use File::Spec;
use Test::More;
use Sys::Hostname 'hostname';

=head1 NAME

git-repos.t - periodic sanity checks for central Git repos

=head1 DESCRIPTION

This is to ward off the evil spirits of non-fast-forward pushes and
pick up any other configuration problems.

=cut

my $host = hostname();
if (@ARGV && $ARGV[0] eq 'remote') {
    # Caller already checked for skipping.  We plan.
    plan tests => 1;
    diag("Running remotely on $host");
    $ENV{PATH} = '/bin:/usr/bin';
    denynonfastforwards_t();
} else {
    # Initial call under prove.  If we are to test, it must be remote.
    diag("Initial run on $host");

    # Taint clearance, and independence from $PWD
    $ENV{PATH} = '/bin:/usr/bin';
    my $script = File::Spec->rel2abs($0);
    die "Detaint fail on $script"
      unless $script =~ m{^(/[-_a-zA-Z0-9/]+\.?[a-z]*)$};
    $0 = $1;

    # Avoid the need for finding the extra module on remote side.
    require Test::Sometimes;
    Test::Sometimes::skip_sometimes( 5 * 86400 ); # every 5 days
    remote_test("intcvs1");
}

sub remote_test {
    my ($host) = @_;
    exec "ssh", -Tax => $host, $0, "remote"; # assume NFS filesystem
}

sub denynonfastforwards_t {
    open my $fh, "-|",
      "find /repos/git/anacode /repos/git/annotools -type f -name config -print0 | xargs -r0 grep -Li -E '^[^#]*denynonfastforwards = true'"
        or die "Pipe from find pipeline: $!";
    my @problem = <$fh>;
    close $fh;

    is("", join '', @problem, "Configs should set denynonfastforwards");
}
