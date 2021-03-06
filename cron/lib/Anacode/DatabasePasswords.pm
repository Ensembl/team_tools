package Anacode::DatabasePasswords;

use strict;
use warnings;

=head1 NAME

Anacode::DatabasePasswords - utility module to get password

=head1 DESCRIPTION

The common routes to passwords are

=over 4

=item *

Fetch from Otter data server.  This is preferred in most cases, but
does not contain everything and should not be relied on if we are
doing system sanity testing.

=item *

Hardcoded in scripts.  This is quite common, but not good practice.

=item *

Some developers have used Netrc and copy/pasted logic to drive it per
script.  This doesn't work well with C<ottro>.

=item *

This module is another way.  Time will tell whether it's a good idea.
It uses whatever is available...  maybe.

=back

This module currently expects "user:pass" lines in F<.dbpass> in the
top directory of the repository.  The file must not be publicly readable.

=head1 EXPORTABLE FUNCTIONS

=cut

use Carp;

use base 'Exporter';
our @EXPORT_OK = qw( user_password );


=head2 user_password($user)

Returns ($user, $pass) or generates an error.  $pass may be an empty
string.

=cut

sub user_password {
    my ($user) = @_;

    # shortcut the simple and well-known
    return ($user, "") if $user eq 'ensro' || $user eq 'ottro';

    return ($user, _up($user));
}

{
    my %user2pass; # key = user, value = password (assumed the same for all databases)

    sub _up {
	my ($user) = @_;

	# use an ignored file in this project
	my $passfn = __FILE__;
	$passfn =~ s{/cron/lib/.+$}{/.dbpass} or die "Cannot make path to top of repo from $passfn";

	if (!%user2pass) {
	    %user2pass = _readfile($passfn);
	}

	if (exists $user2pass{$user}) {
	    return $user2pass{$user};
	} else {
	    my @know = sort keys %user2pass;
	    croak "I have no password for user $user, only (@know)\n  Please 'echo $user:mumblepassword >> $passfn'";
	}
    }
}

sub _readfile {
    my ($passfn) = @_;

    my @out;
    if (open my $fh, "<", $passfn) {
	_checkpriv($passfn);
	foreach my $ln (<$fh>) {
	    chomp $ln;
	    next if $ln =~ /^#|^\s*$/; # ignore traditional comments
	    if ($ln =~ m{^([^:]+):(.*?)\s*$}) {
		push @out, $1, $2;
	    } else {
		warn "$passfn: Ignoring incomprehensible line";
	    }
	}
    } else {
	warn "Cannot read $passfn: $!, I have no passwords";
    }
    return @out;
}

# Die if $fn has public access, to avoid accidents.  Otherwise, ignore
# any group permissions or ACLs - assume that they are deliberate.
sub _checkpriv {
    my ($fn) = @_;
    my @s = stat($fn);
    if ($s[2] & 07) {
	die sprintf("%s: is mode 0%03o but must not be public",
		    $fn, $s[2] & 07777);
    }
}

1;
