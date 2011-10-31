#! /usr/bin/perl -T

use strict;
use warnings;
use YAML 'Dump';

sub main {
  $ENV{PATH} = '/bin:/usr/bin';

  die "Syntax: $0 <mail alias>\n" unless
    1 == @ARGV && $ARGV[0] =~ m{^([a-zA-Z][-a-zA-Z0-9]*)$};
  my $group = $1;

  my @user = split /\s*,\s*/, qx{ /usr/bin/ypmatch $group aliases };
  die "No users found from ypmatch" unless @user;
  chomp @user;

  my %user; # key = username, value = otterdir
  foreach my $u (@user) {
    my $home = uhome($u);
    next unless defined $home;
    my $ottdir = "$home/.otter";
    next unless -d $ottdir;
    $user{$u} = "$home/.otter";
  }
  warn sprintf("Found %d users with %d ~/.otter directories\n", scalar @user, scalar keys %user);
  print Dump({ users => \%user });
}

sub uhome {
  my ($u) = @_;
  my @u = getpwnam($u);
  if (!@u) {
    warn "Not a user? '$u'\n";
    return ();
  }
  return $u[7];
}

main();
