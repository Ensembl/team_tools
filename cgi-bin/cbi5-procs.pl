#! /usr/bin/perl -T

use strict;
use warnings;


=head1 NAME

cbi5d-procs.pl - mysql show full processlist

=head1 DESCRIPTION

Display running queries on database.

=head1 AUTHOR

Matthew Astley, team119 (Anacode)

=cut


use Sys::Hostname 'hostname';
use DBI;
use CGI 'escapeHTML';

$ENV{PATH} = '/bin:/usr/bin:/sbin:/usr/sbin';


my ($user, $pass) = (genero => undef); # cannot see all processes

my $SHOWPROCS = q{show full processlist};

sub headlinks {
    my $me = (getpwuid($<))[0];
    $me = "\u$me";
    return <<HTML;
<p>
  [ <a href="/"> top index </a>
  | <a href="http://mediawiki.internal.sanger.ac.uk/wiki/index.php/User:$me">[[User:$me]]</a>
  | <a href="http://ganglia.internal.sanger.ac.uk/gweb-2.2.0/?c=Lutra%20Servers&m=load_one&r=hour&s=by%20name&hc=4&mc=2">lutras on Ganglia</a>
  | <a href="http://git.internal.sanger.ac.uk/cgi-bin/gitweb.cgi?p=users/mca/local-apache.git;a=history;f=cgi-bin/mysql-procs.pl;hb=HEAD"> this code </a>
  | <a href="http://mediawiki.internal.sanger.ac.uk/wiki/index.php/Anacode_Databases">[[Anacode Databases]]</a>
  ]
</p>
HTML
}

sub css_text {
    return <<'CSS';
 .null		{ font-family: monospace; font-size:75%; font-style:italic; color: grey }

 tt.host	{ white-space: nowrap; font-size: 85% }
 .web		{ color: purple }
 .dev		{ color: #b50 } /* dark orange */

 div.age	{ white-space: nowrap; float: right; padding-left: 1em }
 pre.sql	{ margin: 0; white-space: pre-wrap }
 span.mode	{ font-size: 140%; color: brown; float: right }

 tr.self td	{ color: grey; text-decoration: line-through }
 tr.sleep td	{ color: #228 }
 tr.query td	{ color: black }
CSS
}


sub main {
    print <<HDR;
Content-type: text/html\n
<head>
 <title>cbi5-procs (Anacode)</title>
 <style type="text/css">
@{[ css_text() ]}
 </style>

 <!-- pasted indiscriminately from SangerWeb -->
 <link rel="stylesheet" type="text/css" href="http://wwwdev.sanger.ac.uk/css/wtsi.css?2007" />
 <script type="text/javascript" src="http://jsdev.sanger.ac.uk/prototype.js" ></script>
 <script type="text/javascript" src="http://jsdev.sanger.ac.uk/scriptaculous/scriptaculous.js" ></script>
 <script type="text/javascript" src="http://jsdev.sanger.ac.uk/sidebar.js" ></script>
 <script type="text/javascript" src="http://jsdev.sanger.ac.uk/toggle.js" ></script>
 <script type="text/javascript" src="http://jsdev.sanger.ac.uk/urchin.js" ></script>
 <script type="text/javascript" src="http://jsdev.sanger.ac.uk/zebra.js" ></script>

</head><body>
 <div id="main" class="expanded"><div id="content"><!-- for SangerWeb style -->

@{[ headlinks() ]}
HDR

    print <<'TBL';

<button id='hidesql' onclick="       Effect.multiple( $$('pre.sql'), Effect.SwitchOff, { speed: 0 }); Effect.SwitchOff($('hidesql'))
"> <noscript>(requires JavaScript)</noscript> Hide SQL text </button>
<button id='hidesleeper' onclick="   Effect.multiple($$('tr.sleep'), Effect.SwitchOff, { speed: 0 }); Effect.SwitchOff($('hidesleeper'))
"> <noscript>(requires JavaScript)</noscript> Hide sleeping processes </button>

<table frame="box" rules="all" class="queries" >
 <colgroup>
  <col span="7" width="10ex">
  <col id="sqlcol">
 </colgroup>
TBL

    show_databases();
    print qq{</table>\n};

#    my $n = `netstat -ntp; ps faux` || "(failed to get connection + process lists)";
#    my $hn = escapeHTML($n);
#    my $hostname = escapeHTML(hostname());
#    print "<h1>Client info for $hostname</h1>\n<pre>$hn</pre>\n";
#
###  Turns out the connections held open for hours are from proserver (DAS)

    print qq{</div></div></body></html\n};
}

sub show_databases {
    my @db = qw( cbi5d:3306 );

    print "<p>[", (join " | ", map { my $h = escapeHTML($_); qq{<a href="#$_">$_</a>} } @db), "]</p>\n";

    $| = 1;
    foreach my $db (@db) {
	print show_conn($db);
    }
}


sub headings {
    return ("<tr>", (map {"<th>$_</th>"} qw{ Id User Host db Command Time State Info }), "</tr>\n");
}


sub show_conn {
    my ($dbinst) = @_;

    my ($host, $port) =
      $dbinst =~ m{^([-a-z0-9.]+):(\d+)$}i or
	die "Can't parse dbinst '$dbinst'";

    my $dsn = "DBI:mysql:host=$host;port=$port";

    my $code = sub {
#	my $dbh = DBI->connect($dsn, $user, $pass, { RaiseError => 1 });
	my $dbh = eval { DBI->connect($dsn, $user, $pass, { RaiseError => 1 }) }
	  || DBI->connect($dsn, $user, $pass."!!", { RaiseError => 1 }); # XXX: temporary! mca 2011-01-25

#    my $n = `ss -torp dst $host:$port` || "(failed to get connection list)"; # ss not available on webserver
#    my $hn = escapeHTML($n);
#    print "<pre>$hn</pre>\n";

	return ($dbh->selectall_arrayref($SHOWPROCS),
		$dbh->selectall_arrayref(q{show global variables}),
		$dbh->selectall_arrayref(q{show status}));
    };
    my ($err, $conns, $vars, $status) = run_with_timeout($code);

    my $hdbinst = escapeHTML($dbinst);

    my $hvars = '';
    my $RO = '';
    if (defined $vars) {
	my %vars = map {($_->[0], $_->[1])} @$vars; # rows of (key, value)
	$RO .= ' <span class="mode">read only</span>' if $vars{'read_only'} ne 'OFF';
	$hvars = join ", ", map {escapeHTML("$_ = $vars{$_}")} qw(datadir read_only version);
    }

    my $hstatus = '';
    if (defined $status) {
	my %status = map {($_->[0], $_->[1])} @$status; # rows of (key, value)
	my $up = $status{'Uptime'};
	my $d = int($up / 86400); $up -= $d * 86400;
	my $h = int($up /  3600); $up -= $h *  3600;
	$RO .= ' <span class="mode">slave</span>' if $status{'Slave_running'} eq 'ON';
	$hstatus = join ", ", sprintf('Up %dd %dh %dm', $d, $h, int($up / 60)),
	  map {escapeHTML("$_ = $status{$_}")} qw( Slave_running );
    }

    my $hdr = qq{ <tr><th colspan="3"> $RO <a name="$hdbinst"> <h2>$hdbinst</h2> </a></th><td colspan="4">$hvars</td><td>$hstatus</td></tr>\n};

    if ($err) {
	my $herr = escapeHTML($err);
	return ($hdr,
		qq{ <tr><td colspan="8"><pre class="fail">$herr</pre></td></tr>\n});
    } else {
	return ($hdr,
		headings(),
		map { connrow(@$_) } sort { $a->[5] <=> $b->[5] } @$conns);
    }
}

sub connrow {
    my ($id, $q_user, $host, $db, $command, $time, $state, $info) = my @row = @_;

    # Pick a <tr class>
    my $class;
    if ($q_user eq $user && !defined $db && $command eq 'Query' && $info eq $SHOWPROCS) {
	$class = "self";
    } elsif ($command eq 'Sleep' && !defined $info) {
	$class = "sleep";
    } else {
	$class = "query";
    }

    # Format up some cell contents
    my @hostclass = ("host");
    push @hostclass, "web" if $host =~ /^web(-|dev)/;
    push @hostclass, "dev" if $host =~ /^(deskpro|mib\d)/;
    $row[2] = [ qq{<tt class="@hostclass">}.escapeHTML($host)."</tt>" ];
    $row[5] = [ escapeHTML($time).q{ <div class="age">}.escapeHTML(age($time))."</div>" ];
    if (defined $info) {
	$info =~ s/\A\s+|\s+\z//gs;
	$row[7] = [ q{<pre class="sql">}.escapeHTML($info)."</pre>" ];
    }

    # Build the row
    return (qq{ <tr class="$class"><td>},
	    (join "</td><td>", map { TDify($_) } @row),
	    "</td></tr>");
}

sub TDify {
    my ($cell) = @_;

    return q{<code class="null">NULL</code>} unless defined $cell;
    return @$cell if ref($cell) eq 'ARRAY'; # hack to negate escapeHTML
    return escapeHTML($cell);
}

sub age {
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


# The usual $SIG{ALRM} trick doesn't always work for DBI.
# This is from the manpage.
sub run_with_timeout {
    my ($code, @args) = @_;

    use v5.8.0;
    use POSIX ':signal_h';

    my $mask = POSIX::SigSet->new( SIGALRM ); # signals to mask in the handler
    my $action = POSIX::SigAction->new
      (sub { die "timeout" },        # the handler code ref
       $mask,
       # not using (perl 5.8.2 and later) 'safe' switch or sa_flags
      );

    my $oldaction = POSIX::SigAction->new();
    sigaction(SIGALRM, $action, $oldaction);

    my @ret;
    eval {
	alarm(2); # seconds before time out
	@ret = $code->(@args);
	alarm(0); # cancel alarm (if connect worked fast)
    };
    alarm(0);    # cancel alarm (if eval failed)
    sigaction(SIGALRM, $oldaction);  # restore original signal handler

    my $err = $@;
    return ($err, @ret);
}

main();
