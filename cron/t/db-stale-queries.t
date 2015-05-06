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

=head1 CAVEATS

This code is (already!) at the point of needing re-engineering.
Cooking the "SHOW PROCESSLIST" results into an object per row could
make things much clearer.

=cut

my %slave = (otterlive => "mcs18",
             mcs17     => "mcs18",);

my %known =
  (psl_fetch => q{SELECT matches, misMatches, repMatches, nCount, qNumInsert,
qBaseInsert, tNumInsert, tBaseInsert, strand, qName, qSize, qStart,
qEnd, tName, tSize, tStart, tEnd, blockCount, blockSizes, qStarts,
tStarts FROM (\S+) WHERE tName = '(\S+)' AND tEnd >= '(\S+)' AND
tStart <= '(\S+)' ORDER BY tStart ASC},
   psl_dna => q{SELECT dna FROM (\S+_psl_dna) WHERE qName = '([^']+)'},


   otter_analyses => q{
   SELECT i.input_id
     , a.logic_name
     , i.created
     , i.db_version
   FROM analysis a
     , input_id_analysis i
   WHERE a.analysis_id = i.analysis_id
     AND i.input_id IN (\([^()]+\)) },
  );

sub main {
    %known = map {( $_ => sql_to_regexp_hack($known{$_}) )} keys %known;

    foreach my $hostport (qw(  otterlive:3324 mcs17:3322 mcs17:3323 )) {
        my ($user, $pass) = user_password("ottadmin");

	my $dbh = try_connect($hostport, $user, $pass, 0);
        my @zap;
        push @zap, check_stale([ host_port($hostport, 0) ], $dbh);

        $dbh = try_connect($hostport, ottro => '', 1);
        push @zap, check_stale([ host_port($hostport, 1) ], $dbh);

        zap_maybe(0, @zap);
#        print Dump(\@zap);
    }
}


sub zap_maybe {
    my ($yes, @q) = @_;

    my @show;
    foreach my $q (sort { $a->{Time} <=> $b->{Time} } @q) {
        push @show, sprintf "kill %6d -- %-12s since %8s, on %s @ %s:%s\n",
          @{$q}{qw{ Id What Age db }}, @{ $q->{Server} };
        unshift @show, sprintf qq{mysql -h%s -P%d -uottro -e 'kill %d' %s\n},
          @{ $q->{Server} }, @{$q}{qw{ Id db }};
        # XXX:TODO print a useful ANALYZE TABLE statement to go with the kill
        diag Dump({ What => $q->{What},
                    Detail => $q->{Args} || $q->{Info} });
    }
    print STDERR @show;
}


sub check_stale {
    my ($server, $dbh) = @_;
    my @slow_web_q; # targets for kill

    my $web_timeout = 5 * 60;

  SKIP: {
        skip "no connection", 1 unless $dbh; # other tests will have FAILed

        my $SHOWPROCS = 'show full processlist';
        my $procs = $dbh->selectall_arrayref($SHOWPROCS);

        my %q;
        while (my $row = shift @$procs) {
            my %r;
            @r{qw{ Id User Host db Command Time State Info }} = @$row;
            if (!defined $r{Time} && !defined $r{Info} && !defined $r{db} &&
                $r{Command} eq 'Connect' &&
                $r{User} eq 'unauthenticated user' &&
                $r{State} =~ /^(Reading from|Writing to) net$/) {
                # Connection has just started - nothing to learn
                next;
            }
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
                    "slow_web queries on @$server");
#        diag Dump(\%q) unless $ok;

        push @slow_web_q, @{ $q{slow_web} || [] };
        foreach my $r (@slow_web_q) {
            $r->{dbh} = $dbh;
            ($r->{What}, $r->{Args}) = guess_query($r->{Info});
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
    my ($hostport, $slave) = @_;
    if (my ($h, $p) = $hostport =~ /^(.*):(\d+)$/) {
        $h = $slave{$h} if $slave;
	return ($h, $p);
    } else {
	die "Incomprehensible hostport $hostport";
    }
}

sub dsnify { # XXX:DUP changed from otter_databases.t
    my ($hostport, $slave) = @_;
    my ($h, $p) = host_port($hostport, $slave);
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

sub sql_to_regexp_hack {
    my ($v) = @_;

    # SQL-specific text->regexp munging; some arbitrary rules
    # intended to be convenient
    $v =~ s{(\w)\.(\w)}{$1\\.$2}g; # select table.column
    $v =~ s{\s*,\s*}{\\s*,\\s*}g; # select foo , bar
    $v =~ s{\A\s*}{\\A\\s*};
    $v =~ s{\s*\Z}{\\s*\\z};
    $v =~ s/\s+/\\s+/g;
    return qr{$v};
}

sub guess_query {
    my ($sql) = @_;
    return () unless defined $sql;

    while ( $sql =~ s{\A\s*-- .*$}{}m or
            $sql =~ s{\A\s*/\*.*?\*/}{}s) {
        # strip leading comments
    }

    () = keys %known; # reset each%
    # It didn't finish last time!
    while (my ($k, $v) = each %known) {
        my @match = $sql =~ $v;
        return ($k, \@match) if @match;
    }

    my ($op) = $sql =~ m{^\s*(\S+)};
    return ($op || 'non-SELECT query');
}

main();
