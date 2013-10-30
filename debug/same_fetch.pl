#! /software/bin/perl-5.12.2
use strict;
use warnings;

use YAML 'Dump';
use HTTP::Async::Polite;
use HTTP::Request;
use Digest::SHA 'sha1_hex';
use File::Temp 'tempdir';
use File::Slurp 'write_file';


sub main {
    open my $logs_fh, '-|', qw( webvm-logs --cat *access* )
      or die "Pipe logs: $!";
    $| = 1;

    my $cookie = 'WTSISignOn='.die("me want cookie");

    my $ua = HTTP::Async::Polite->new
      (send_interval => 0.25,
       slots => 8,
       req_per_host => 3,       # a local hack
       timeout=> 100);

    my @op_whitelist = qw(
        authenticate_me
        find_clones
        get_analyses_status
        get_assembly_dna
        get_clonesequences
        get_config
        get_datasets
        get_db_info
        get_gff_das_features
        get_gff_features
        get_gff_funcgen_features
        get_gff_genes
        get_gff_patch_features
        get_gff_psl_sql_features
        get_locks
        get_mapping
        get_meta
        get_region
        get_sequence_notes
        get_sequencesets
                        );

    my $outdir = tempdir("same_fetch.XXXXXX", TMPDIR => 1, CLEANUP => 0);
    my %qy; # key = query, value =
    # { orig => { $log_status => \@size },
    #   resp => { "$sha1_hex $http_status_code $leng" => $count },
    # }
    my @req; # index = id, value = query
    while (<$logs_fh>) {
        my $orig = $_;
        next if m{ \[(\d+/Sep/2013|[01]\d/Oct/2013)}; # before test release
        s/^.*?"// or die "Can't trim leader off $_";

        # We study only Otter Server
        next unless m{/cgi-bin/otter};

        my ($method, $q, $status, $len) =
          m{^([A-Z]+) (.*?) HTTP/1\.1" (\S+) (\S+) .*}
            or die "Can't extract for $_";
        my ($vsn, $op) = $q =~
          m{^/cgi-bin/otter(?:~[^/]+)?/(\d+)/(.*?)(?:\?|$)}
            or die "Can't extact op from $q";

        next if $op =~ /^set_|lock_region$/; # do not cause side effects!

        # Not going to learn anything
        next if $status eq 403;
        next if m{/cgi-bin/selftest/}; # fetches made by selftests
        next unless $method eq 'GET'; # we lack the content
        next if $op eq 'test';

        unless (grep { $op eq $_ } @op_whitelist) {
            warn "Skip unknown op: $op\n";
            next;
        }

        my @call = qw( http://dev.sanger.ac.uk http://otter.dev.sanger.ac.uk );

        # Initiate fetches (first time only)
        @call = () if exists $qy{$q}; # seen it
        foreach my $base (@call) {
            my $req = HTTP::Request->new
              (GET => "$base$q",
               [qw[ User-Agent team_tools.git/debug/same_fetch.pl ],
                Cookie => $cookie ]);
            my $id = $ua->add($req);
            $req[$id] = $q;
        }

        push @{ $qy{$q}{orig}{$status} }, $len;
    }

    $SIG{INT} = sub {
        if ($ua->to_send_count) {
            $ua->clear_send_queue;
            warn "Caught SIGINT - cleared request queue\n";
        } else {
            die "Caught SIGINT, send queue empty.  Boom!\n";
        }
    };
    while ($ua->not_empty) {
        my ($resp, $id) = $ua->wait_for_next_response(0.5);

        # Progress
        printf("< %s %s\n", $resp->code, $resp->request->uri) if $resp;

        my $txt = $ua->info;
        $txt =~ s{\n}{\r};
        print "# $txt";
        next unless $resp;

        # Got response
        my $q = $req[$id];
        my $sha = ensure_put($outdir, $resp);

        my $key = join ' ', $sha, $resp->code, length($resp->decoded_content);
        $qy{$q}{resp}{$key} ++;
        $qy{$q}{DIFF} = 1 if keys %{ $qy{$q}{resp} } > 1;
    }

    my $smry_fn = "$outdir/summary.yaml";
    write_file($smry_fn, { atomic => 1 },
               Dump({ qy => \%qy, req => \@req, outdir => $outdir }));
    warn "Wrote $smry_fn    \n";

    return 0;
}


sub ensure_put {
    my ($outdir, $resp) = @_;
    my $sha = sha1_hex($resp->decoded_content);

    my $fn = "$outdir/$sha";
    unless (-f $fn) {
        write_file($fn, { atomic => 1 }, $resp->decoded_content);
    }

    return $sha;
}


sub HTTP::Async::clear_send_queue { # another local hack
    my ($self) = @_;
    @{ $$self{to_send} } = (); # base class
    @{ $$self{domain_stats}{$_}{to_send} } = () # polite
      for keys %{ $$self{domain_stats} };
    return;
}


exit main();
