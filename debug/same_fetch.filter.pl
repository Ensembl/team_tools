#! /software/bin/perl-5.12.2
use strict;
use warnings;

use YAML qw( Dump LoadFile );
use List::MoreUtils 'uniq';
use File::Slurp 'slurp';
use Digest::SHA 'sha1_hex';


# This takes YAML from ./same_fetch.pl and munges it, to help me
# understand what might be broken.
#
# Do not maintain it.

sub main {
    my ($fn) = @_;
    my (%qy, @req);
    {
        my $info = LoadFile($fn);
        die "bad input $fn" unless eval { $info->{req} && $info->{qy} };
        %qy = %{ $info->{qy} };
        @req = @{ $info->{req} };
    }

    # Drop the happies
    my %cause;
    my @ok;
    while (my ($q, $v) = each %qy) {
        my $resp = $v->{resp};
        if (! $v->{DIFF} &&
            (join ',', keys %{ $v->{orig} }) eq '200' && # original was OK
            (join ',', values %$resp) eq '2' &&          # fetches unanimous
            (keys %$resp)[0] =~ /^\S+ 200 \d+$/) {       # fetches OK
            delete $qy{$q};
            push @ok, $q;
        } elsif (my ($problem, @err) = diagnose($resp)) {
            delete $v->{DIFF};
            $v->{ERROR} = \@err;
            $cause{"problem:$problem"}{$q} = $resp;
#            delete $qy{$q};
        } elsif ($q =~ m{^/cgi-bin/otter/77/get_gff_psl_sql_features\?}) {
            # no real problem, the diff was attribute ordering
            delete $qy{$q};
            push @ok, $q;
        } else {
            my @status = map { /^\S+ (\d{3}) \d+$/ ? $1 : die "bad resp $_" } keys %$resp;
            my @fn     = map { /^(\S+) \d{3} \d+$/ ? $1 : die "bad resp $_" } keys %$resp;
            my $diff;
            $diff = `diff -U0 @fn` if 2 == @fn;
            my $now = join ',', sort @status;
            my $was = join ',', keys %{ $v->{orig} };
            if (defined $diff) {
                if ($diff =~ m{Class "Sequence";Name "NONAME\?"}) {
                    # the config generated the URL, so will now be broken on old webserver
                    $now = 'config_fixed';
                } else {
                    $v->{DIFF} = $diff;
                }
            }
#            delete $qy{$q} if $now eq 'config_fixed';
            $cause{"was $was; now $now"}{$q} = $resp;
        }
    }

    @req = grep { $qy{$_} } @req;
    @req = uniq(sort @req);

    print Dump({ req => \@req, qy => \%qy,
                 summary => { n_req => scalar @req,
                              n_qy => scalar keys %qy },
                 cause => \%cause,
                 ok => \@ok,
               });

    return 0;
}

# For the ones that errored,
# did other args give same?

sub diagnose {
    my ($resp) = @_;

    # Get filenames
    my @resp = keys %$resp;
    $resp = join "\n", @resp;
    my @fn = $resp =~ m{^([0-9a-f]{40}) }mg;
    die "Failed to get fn(@fn) from resps($resp)" unless @fn == @resp;

    # Exclude terse failures
    my @fail;
    my @old_fn = @fn;
    @fn = ();
    foreach (@old_fn) {
        my $fail;
        $fail = 'DAS_500' if /^4bdc84b11cc1e0fb3b2d38b6f951b427c987fec5$/;
        # ...'ERROR: Error from DAS request: 500 Internal Server Error'...

        $fail = 'timeout' if /^edcd3630837b7df456c294fe53fde8ee926673bc$/;
        # 'Timed out'

        $fail = 'gencode_human_h37=undef' if
          /^(e8cf27485140698fa13ad5279284f9340ce9cd38
           |ed40d99a9dc419ea9d4b9dfd602b5455995f1c52)$/x;
        # 'ERROR: metakey 'gencode_human_h37' is not defined at' ...

        $fail = 'need_dataset' if
          /^(4da2341e3a49f297557aaf7a37ef0b8e83a19b67
           |8380806d9e1bb1e375839400e2a82e9ef2c9b6c1)$/x;
        # 'ERROR: No 'dataset' argument defined at'...

        push @{ $fail ? \@fail : \@fn }, $_;
    }

    # Read & collect
    my $are_diff = (2 == @fn && $fn[0] ne $fn[1]);
    my @trace;
    my %gff_sha; # key=sha1, val=count
    foreach my $fn (@fn) {
        die "Can't find ./$fn" unless -f $fn;
        my $txt = slurp($fn);
        if (my ($unwrap) = $txt =~
            m{^<\?xml[^<>\n]+>\n *<otter>\n *<response>\n(.*\n) *</response>\n *</otter>\n*$}s) {
            my @stack = $unwrap =~ m{^STACK (.*)}mg;
            $unwrap =~ s{^---+ *EXCEPTION *--+$}{}m;
            $unwrap =~ s{^---+$}{}m;
            $unwrap =~ s{^STACK.*\n}{}m;
            push @trace, $unwrap if @stack > 3;
        } elsif ($are_diff && $txt =~ /^##gff-version 2/) {
            system("$ENV{ANACODE_TEAM_TOOLS}/debug/gff_canonicalise.pl $fn")
              && die "gff_canonicalise failed on $fn: !=$! ?=$?";
            my $txt = slurp("${fn}_canon.gff");
            $gff_sha{ sha1_hex($txt) } ++;
        }
    }

    # two GFFs are equivalent
    return (gff_sort => @fn, keys %gff_sha)
      if $are_diff && (join ',', values %gff_sha) eq @fn;

    # both are stacktraces (content will differ),
    # or one stacktrace and one timeout
    return (trace => @trace) if @trace == @fn && @trace;

    # both timeout
    return (terse_fail => @fail) if 2 == @fail || (1 == @fail && 0 == @fn);

    # other - undetermined
    return ();
}

exit main(@ARGV);
