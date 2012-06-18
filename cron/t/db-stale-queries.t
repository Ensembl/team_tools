#! /usr/bin/env perl

use strict;
use warnings;

use Test::More tests => 6;
use DBI;

use Anacode::DatabasePasswords qw( user_password );
use YAML 'Dump';


=head1 NAME

db-stale-queries.t - find (and fix) too-slow-for-CGI queries

=head1 DESCRIPTION

If the C<SELECT> issued by a CGI script is still running when Apache
given up and killed the process, continuing to run the query is futile
and has been a source of snowballing database load.

=cut

my %slave = (otterlive => "otlpslave",
             otterpipe1 => "otp1slave",
             otterpipe2 => "otp2slave");

my %known =
  (psl_fetch => q{SELECT matches, misMatches, repMatches, nCount, qNumInsert,
qBaseInsert, tNumInsert, tBaseInsert, strand, qName, qSize, qStart,
qEnd, tName, tSize, tStart, tEnd, blockCount, blockSizes, qStarts,
tStarts FROM (\S+) WHERE tName = '(\S+)' AND tEnd >= '(\S+)' AND
tStart <= '(\S+)' ORDER BY tStart ASC},
  );

sub main {
    my @want = # list of [ host:port, user, pass, \@morechecks ]
      ([ "otterlive:3324", user_password("ottadmin") ],
       [ "otterpipe1:3322", user_password("ottadmin") ],
       [ "otterpipe2:3323", user_password("ottadmin") ],
      );

    foreach my $wantrow (@want) {
	my ($hostport, $user, $pass, @morechecks) = @$wantrow;


	my $dbh = try_connect($hostport, $user, $pass);
        my @zap;
        push @zap, check_stale($hostport, $dbh);

        $dbh = try_connect($hostport, ottro => '', 1);
        push @zap, check_stale("$hostport slave", $dbh);

        zap_maybe(0, @zap);
    }
}


sub zap_maybe {
    my ($yes, @q) = @_;

    foreach my $q (sort { $a->{Time} <=> $b->{Time} } @q) {
        printf "kill %d; -- %s on %s, %s\n", @{$q}{qw{ Id db Server Age }};
        diag Dump($q->{What}) if $q->{What};
    }
}


sub check_stale {
    my ($server, $dbh) = @_;
    my @slow_web_q; # targets for kill

    my $web_timeout = .05 * 60;

  SKIP: {
        skip "no connection", 1 unless $dbh; # other tests will have FAILed

        my $SHOWPROCS = 'show full processlist';
        my $procs = $dbh->selectall_arrayref($SHOWPROCS);

        my %q;
        while (my $row = shift @$procs) {
            my %r;
            @r{qw{ Id User Host db Command Time State Info }} = @$row;
            $r{Age} = age($r{Time});
            $r{Server} = $server;
            my ($id, $q_user, $host, $db, $command, $time, $state, $info) = @$row;
            my $type;
            if ($r{User} =~ m{^(root|slave)$} ||
                ($r{Command}||'') eq 'Sleep' ||
                ($r{Info}||'') eq $SHOWPROCS # self
               ) {
                $type = 'ignore';

            } elsif ($time < 10) {
                $type = 'quick_so_far';

            } elsif ($time < $web_timeout || $host !~ m{www|\bweb}) {
                # look for web machines; but not ensweb because they're Vega
                $type = 'running';

            } elsif ($r{Info} && $r{Info} =~ m{^\s*SELECT\s+}i) {
                $type = 'slow_web';

            } else {
                $type = 'web_nonSELECT';
            }

            push @{ $q{$type} }, \%r;
        }

        my $ok = is(scalar @{ $q{slow_web} || [] }, 0,
                    "slow_web queries on $server");
#        diag Dump(\%q) unless $ok;

        push @slow_web_q, @{ $q{slow_web} || [] };
        foreach my $r (@slow_web_q) {
            $r->{dbh} = $dbh;
            $r->{What} = guess_query($r->{Info});
        }
    }

    die "need array context" unless wantarray;
    return @slow_web_q;
}


sub try_connect { # XXX:DUP changed from otter_databases.t
    my ($hostport, $user, $pass, $slave) = @_;

    my $dbh = eval {
        DBI->connect(dsnify($hostport, $slave),
                     $user, $pass,
                     { RaiseError => 1, AutoCommit => 0 });
    };
    my $err = $@;
#    ok(ref($dbh), "Connect to $hostport as $user");
    diag($err) if $err;
    return $dbh;
}


sub host_port { # XXX:DUP otter_databases.t
    my ($hostport) = @_;
    if ($hostport =~ /^(.*):(\d+)$/) {
	return ($1, $2);
    } else {
	die "Incomprehensible hostport $hostport";
    }
}

sub dsnify { # XXX:DUP changed from otter_databases.t
    my ($hostport, $slave) = @_;
    my ($h, $p) = host_port($hostport);
    $h = $slave{$h} if $slave;
    return "DBI:mysql:host=$h;port=$p";
}

sub age { # XXX:DUP mysql-procs.pl
    my ($sec) = @_;
    my @age;

    foreach my $U ([ d => 86400 ], [ h => 3600 ], [ m => 60 ]) {
	my ($sym, $sz) = @$U;
	my $num = int( $sec / $sz );
	if (@age || $num) {
	    $sec -= $num * $sz;
	    push @age, "$num$sym";
	}
    }

    return join " ", @age, "${sec}s";
}

sub guess_query {
    my ($sql) = @_;
    return () unless defined $sql;

    while (my ($k, $v) = each %known) {
        if (!ref($v)) {
            $v =~ s{^\s*}{^\\s*};
            $v =~ s{\s*$}{\\s*\$};
            $v =~ s/\s+/\\s+/g;
            $known{$k} = $v;
        }
        my @match = $sql =~ $v;
        return [$k, @match] if @match;
    }

    return ();
}

main();
