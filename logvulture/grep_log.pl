#! /usr/bin/env perl
use strict;
use warnings;
use File::Slurp 'slurp';

# Fishing in many logfiles for some hit (X11 BadWindow error in this
# case), then wanting the version info off the top when there is a
# hit.
#
#   nice ~/gitwk--bg/team_tools/logvulture/grep_log.pl ./logs.txt:/2=0 -- > XErrors-20140624-all0.log &
#   nice ~/gitwk--bg/team_tools/logvulture/grep_log.pl ./logs.txt:/2=1 -- > XErrors-20140624-all1.log &


my @pat;
my @basic_pat =
  (qr{: git HEAD: humpub-release-\S+\n},
   qr{: Hostname: \S+\n},
   qr{: Tools are( |\n)});

push @pat,
  qr{: (?:(?:(?:X Error|  Major opcode|  Serial number) of|  Resource id in) failed request|  Current serial number in output stream): };


my ($buff, $pos, @basic) = ('');

sub makepats {
    while (@ARGV) {
        my $ptn = pop @ARGV;
        last if $ptn eq '--';
        die "Pattern '$ptn' looks like a filename.  Put a '--' after it"
          if $ptn =~ m{/otterlace\.\d+\S*\.log$} || -e $ptn;
        push @pat, qr{$ptn};
    }
}

my $fnfnpart = '';
sub logfnfile {
    if (1 == @ARGV && $ARGV[0] =~ m{/logs\.txt}) {
        my $logfnfn = shift(@ARGV);
        $logfnfn =~ s{:/(\d+)=(\d+)$}{};
        my ($div, $num) = ($1, $2);

        @ARGV = slurp($logfnfn);
        chomp @ARGV;
        if (defined $div) {
            my $tot = @ARGV;
            my $chunk = int($tot / $div) + 1;
            my ($i, $j) = ($chunk * $num, ($chunk * ($num+1) - 1));
            $j = $#ARGV if $j>$#ARGV;
            @ARGV = @ARGV[ $i .. $j ];
            my $got = @ARGV;
            $fnfnpart = " (part $num of $div)";
            warn "Took $logfnfn $fnfnpart: $tot lines in,  [$i .. $j] = $got lines out.\n";
        }
        next_file(1);
    }
}


sub filter {
    return if $buff eq '';
    if ($. < 500 &&
        grep { $buff =~ $_ } @basic_pat) {
        $buff =~ s{^}{$pos}mg if defined $pos;
        undef $pos;
        push @basic, $buff;
    }
    if (grep { $buff =~ $_ } @pat) {
        $buff =~ s{^}{$pos}mg if defined $pos;
        print splice @basic if @basic;
        print $buff;
    }
    $buff = '';
    undef $pos;
}

sub next_file {
    my ($report) = @_;

    my $n = @ARGV;
    $report = 1 unless $n<20 || $n % 10;
    warn "$n files left to examine$fnfnpart\n" if $report;

    @basic = ();
}

sub main {
    $SIG{INT} = sub { die "Caught SIGINT\n" };
    makepats();
    logfnfile();

    while (<>) {
        if (m{^ {30,}\| }) {
            # continuation line
        } else {
            filter();
        }
        $pos = "$ARGV:$.:" unless defined $pos;
        $buff .= $_;

    } continue {
        if (eof) {  # Not eof()!
            filter();
            next_file();
            close ARGV;
        }
    }
}

main();
