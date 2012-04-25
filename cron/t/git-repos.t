#! /software/bin/perl -T
use strict;
use warnings;

use File::Spec;
use Test::More 0.82; # need correct $TODO behaviour
use Sys::Hostname 'hostname';

=head1 NAME

git-repos.t - periodic sanity checks for central Git repos

=head1 DESCRIPTION

This is to ward off the evil spirits of non-fast-forward pushes and
pick up any other configuration problems.

=cut

sub main {
    local $ENV{PATH} = '/bin:/usr/bin';

    my $host = hostname();
    if (@ARGV && $ARGV[0] eq 'remote') {
        # Caller already checked for skipping.  We plan.
        plan tests => 2;
        diag("Running remotely on $host");
        denynonfastforwards_t();
        public_t();
        return ();

    } else {
        # Initial call under prove.  If we are to test, it must be remote.
        diag("Initial run on $host");

        # Taint clearance, and independence from $PWD
        my $script = File::Spec->rel2abs($0);
        die "Detaint fail on $script"
          unless $script =~ m{^(/[-_a-zA-Z0-9/]+\.?[a-z]*)$};

        local $0 = $1;
        # skip_sometimes also uses $0 to write the stamp file

        # Avoid the need for finding the extra module on remote side.
        require Test::Sometimes;
        Test::Sometimes::skip_sometimes( 5 * 86400 ); # every 5 days
        remote_test("intcvs1");
        die "exec failed"; # we did exec; keep perlcritic happy
    }
}

sub remote_test {
    my ($host) = @_;
    exec "ssh", -Tax => $host, $0, "remote"; # assume NFS filesystem
}

sub denynonfastforwards_t {
    open my $fh, "-|",
      'find /repos/git/anacode /repos/git/annotools -type f -name config -print0'.
        q{ | xargs -r0 grep -Li -E '^[^#]*denynonfastforwards = true'}
          or die "Pipe from find(config) pipeline: $!";
    my @problem = <$fh>;
    close $fh;

  TODO: {
        local $TODO = undef; # it might be TODO, but isn't yet
        my $ffable_re = qr/--BROKEN\b/;
        my @no_excuse = grep { $_ !~ $ffable_re } @problem;
        $TODO = "All are $ffable_re" if @problem && !@no_excuse;

        my $got = join '', @problem;
        chomp $got;
        is($got, '', # expect no output
           '*.git/config: should set denyNonFastforwards');
    }

    return ();
}

sub public_t {
    open my $fh, "-|", find => qw( /repos/git/anacode /repos/git/annotools ),
      qw( ! -perm -o+r -ls -o -type d ! -perm -o+rx -ls )
        or die "Pipe from find(perms) pipeline: $!";
    my @problem = <$fh>;
    close $fh;

    my $got = join '', @problem;
    chomp $got;
    is($got, '', # expect no output
       'some files are not public');

    return ();
}


main();
