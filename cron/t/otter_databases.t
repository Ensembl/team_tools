#! /usr/bin/env perl
use strict;
use warnings;

use Test::More 'no_plan'; # tests => 36; # skipping @morechecks breaks the plan
use DBI;
use YAML 'Dump';

use Anacode::DatabasePasswords qw( user_password );


=head1 NAME

otter_databases.t - Are our databases in the expected state?

=head1 DESCRIPTION

Attempt to connect to all our databases, using relevant usernames.

Spotcheck that ottro cannot write.

Ensure slave databases are not accidentally writable.  (The main aim,
as of first writing.)


=head2 Requirements and side effects

Requires minimal Perl libraries, plus the password file.

Should not change databases unless things are wrong (incorrectly
writable meta table AND not transactional).  Causes lingering test
failure if this does happen, but should not break real apps.

=cut


sub main {
    my @want = # list of [ host:port, user, pass, \@morechecks ]
      ([ "otterlive:3324", user_password("ottro"),
	 [ \&be_readonly, "loutre_human" ] ], # 3 tests
       [ "otterlive:3324", user_password("ottadmin") ],
       [ "otterlive:3324", user_password("ottroot") ],

       [ "otterpipe1:3322", user_password("ottro"),
	 [ \&be_readonly, "pipe_human" ] ],
       [ "otterpipe1:3322", user_password("ottadmin") ],
       [ "otterpipe1:3322", user_password("ottroot") ],

       [ "otterpipe2:3323", user_password("ottro"),
	 [ \&be_readonly, "pipe_pig" ] ],
       [ "otterpipe2:3323", user_password("ottadmin") ],
       [ "otterpipe2:3323", user_password("ottroot"),
#	 [ \&be_readonly, "mca_loutremouse_schema" ], # test would fail because writable
       ],
      );
    my %slave = (otterlive => "otlpslave",
		 otterpipe1 => "otp1slave",
		 otterpipe2 => "otp2slave");

    foreach my $wantrow (@want) {
	my ($hostport, $user, $pass, @morechecks) = @$wantrow;

	my $dbh = want_connect($hostport, $user, $pass);

      SKIP: {
	    skip "no connection" unless $dbh; # we have already one FAIL

	    foreach my $check (@morechecks) {
		my ($code, @args) = @$check;
		$code->("  $hostport as $user", $dbh, @args);
	    }
	}
	$dbh->disconnect if $dbh;

	# check again for the slaves; the password MUST be different
	# except for ottro
	my ($master, $port) = host_port($hostport);
	my $slave = $slave{$master};
      SKIP: {
	    skip "no slave configured", 1 unless $slave;

	    my $dbh2 = want_connect("$slave:$port", $user, enslave_pass($user, $pass));
	    $dbh2->disconnect if $dbh2;
	}
    }
}


sub want_connect {
    my ($hostport, $user, $pass) = @_;

    my $dbh = eval { DBI->connect(dsnify($hostport), $user, $pass, { RaiseError => 1, AutoCommit => 0 }) };
    my $err = $@;
    ok(ref($dbh), "Connect to $hostport as $user");
    diag($err) if $err;
    return $dbh;
}


sub host_port {
    my ($hostport) = @_;
    if ($hostport =~ /^(.*):(\d+)$/) {
	return ($1, $2);
    } else {
	die "Incomprehensible hostport $hostport";
    }
}

sub dsnify {
    my ($hostport) = @_;
    my ($h, $p) = host_port($hostport);
    return "DBI:mysql:host=$h;port=$p";
}

# Password for slave database may be different, to prevent accidental
# loss of sync
sub enslave_pass {
    my ($user, $pass) = @_;
    return $pass if $user eq 'ottro'; # read-only access to slave is not affected
    return $pass."!!"; # the magical "we know what we are doing" password suffix
}


sub be_readonly {
    my ($what, $dbh, $dbname) = @_;

    my $have_db = eval {
	$dbh->do("use $dbname");
	1;
    };
    my $no_db = $@;

  SKIP: {
	skip "$what: $no_db", 2 unless $have_db;

	my $ins = eval {
	    local $SIG{__WARN__} = sub { };
	    $dbh->do("insert into meta (species_id, meta_key, meta_value) values (null, ?,?)", {},
		     "be_readonly.$0", scalar localtime);
	    "Inserted";
	} || "Fail: $@";
	like($ins,
             qr{INSERT command denied to user|MySQL server is running with the --read-only option},
             "$what: Insert to $dbname.meta");
	$dbh->do("rollback");

	my $read = $dbh->selectall_arrayref("SELECT * from meta");
	ok(!@$read || 4 == @{ $read->[0] },
	   "$what: Read $dbname.meta");

	my @was_not_readonly = grep { row_as_text($_) =~ /:be_readonly/ } @$read;
	ok(!@was_not_readonly, "$what: test row is absent");
	diag Dump(\@was_not_readonly) if @was_not_readonly;
    }
}

sub row_as_text {
    my ($row) = @_;
    return join ":", map { defined $_ ? $_ : "(undef)" } @$row;
}

main();
