#! /usr/bin/env perl
use strict;
use warnings;
use File::Slurp 'slurp';
use YAML 'Dump';

# Junk code to trace through RT#395402
#
# For chronological order, run like
#    debug/pid-map.pl ~/../.snapshot/nightly.1/$USER/.otter/otterlace.* | LANG=C sort


my %ps_leftover; # key=pid, value = { HDR => val }
my %oom; # key=pid-killed, value = info

sub readfiles {
    my ($dir) = @_;

    # `ps axl`
    my @txt = slurp("$dir/ps,axl.txt");

    my @head = split /\s+/, shift @txt;
    my %head;
    @head{@head} = (0 .. $#head);
    # F   UID   PID  PPID PRI  NI    VSZ   RSS WCHAN  STAT TTY        TIME COMMAND

    foreach my $ln (@txt) {
        my %ln;
        @ln{@head} = split /\s+/, $ln, 13;
        $ps_leftover{ $ln{PID} } = \%ln;
    }

    # `dmesg | ~mca/bin/uptime-when.pl`
    my %oom_event;
    foreach my $ln (slurp("$dir/dmesg.txt")) {
        chomp $ln;
        if ($ln =~ m{^Booted at \d+ = (\d{4}-\d{2}\S+) localtime$}) {
            my $dt = $1; # 2014-04-10t11:34:38
            $dt =~ s/t/ /;
            print "$dt,0000: SYSTEM BOOT\n";
            next;
        }

        my ($dt, $elapse, $msg) = $ln =~ m{^(\S+): \[([ 0-9]+\.\d+)\] (.*)$}
          or die "Datetime parse fail: $ln";
        # make datetime stamp like Otterlace's
        $dt =~ s{\.(\d{4})\d{2}$}{,$1};
        $dt =~ s/t/ /;

        if ($msg =~ m{^(.*) invoked oom-killer:}) {
            # 2014-04-21t22:21:52.858567: [989234.858567] zmap invoked oom-killer: gfp_mask=0x201da, order=0, oom_adj=0
            my $inv = $1;
            die Dump({ oom_event__dregs => \%oom_event }) if keys %oom_event;
            %oom_event = (dt => $dt, invoker => $inv);
        } elsif ($msg =~ m{^Pid: (\d+), comm: (\S+) }) {
            my $pid = $1;
            die Dump({ oom_event__fail => \%oom_event }) unless 2 == keys %oom_event;
            $oom_event{pid} = $pid;
        } elsif ($msg =~ m{^Out of memory: kill process (\d+) \((\S+)\) score \d+ or a child$}) {
            # 2014-04-21t22:21:52.872633: [989234.872633] Out of memory: kill process 17694 (perl) score 38711 or a child
            my ($pid, $cmd) = ($1, $2);
            die Dump({ oom_event__fail => \%oom_event }) unless 3 == keys %oom_event;
            @oom_event{qw{ or_child_pid or_child_cmd }} = ($pid, $cmd);
        } elsif ($msg =~ m{^Killed process (\d+) \((\S+)\)$}) {
            # 2014-04-21t22:21:52.872640: [989234.872640] Killed process 17706 (perl)
            my ($pid, $cmd) = ($1, $2);
            die Dump({ oom_event__fail => \%oom_event }) unless 5 == keys %oom_event;
            @oom_event{qw{ killed_pid killed_cmd }} = ($pid, $cmd);

            printf "%s:OOMkill: invoked by %s(%s), killed %s(%s) child of %s(%s)\n",
              @oom_event{qw{ dt  invoker pid  killed_cmd killed_pid  or_child_cmd or_child_pid }};
            $oom{ $oom_event{killed_pid} } = { %oom_event };
            %oom_event = ();
        }

    }
}


{
    my %zmap; # key = pid, value = [ start-time, ppid ]
    my %otterlace_exit; # key = pid, value = datetime

    sub z_add {
        my ($o_ppid, $z_pid, $dt) = @_;
        if ($zmap{$z_pid}) {
            printf "%s:%6d: %s\n", $dt, $o_ppid,
              "PID $z_pid re-used (silent ZMap reap), old was @{$zmap{$z_pid}}";
        }
        $zmap{$z_pid} = [ $dt, $o_ppid ];
    }

    sub z_gone {
        my ($o_ppid, $z_pid, $dt, $msg) = @_;
        # o_ppid : could be undef, during END
        return unless $zmap{$z_pid};

        my $ps;
        if ($ps_leftover{$z_pid} &&
            (!defined $o_ppid ||
             $ps_leftover{$z_pid}{PPID} == $o_ppid ||
             $ps_leftover{$z_pid}{PPID} == 1)) {
            $ps = delete $ps_leftover{$z_pid};
        }

        my ($start_dt, $start_ppid) = @{delete $zmap{$z_pid}};
        die "$dt: PID $z_pid died under Otterlace $o_ppid, but started $start_dt under $start_ppid"
          unless !defined $o_ppid || $start_ppid eq $o_ppid;

        $msg .= ", start=$start_dt";
        $msg .= " final_VSZ=$$ps{VSZ}" if $ps;
        printf "%s:%6s: %s\n", $dt, $start_ppid, $msg;
    }

    sub z_infer_gone {
        my ($dt, $o_ppid, @z_pid) = @_;
        my $dt_plus = $dt;
        $dt_plus =~ s{[0-9@]$}{_}; # sort after the causative event
        foreach my $z_pid (sort by_pid @z_pid) {
            z_gone($o_ppid, $z_pid, $dt_plus, "  Leftover ZMap $z_pid");
        }
    }

    sub o_gone {
        my ($dt, $o_ppid) = @_;
        printf "%s:%6d: %s\n", $dt, $o_ppid,
          "Otterlace clean exit";
        $otterlace_exit{$o_ppid} = $dt;
        my @z_pid = grep { $zmap{$_}[1] eq $o_ppid } keys %zmap;
        z_infer_gone($dt, $o_ppid, @z_pid);
    }

    sub o_infer_gone {
        my ($last_dt, $o_ppid) = @_;
        return if $otterlace_exit{$o_ppid};
        $last_dt =~ s{\d,\d{4}$}{_,@@@@}; # time unknown, sort it later
        $otterlace_exit{$o_ppid} = $last_dt;
        my $msg = "Otterlace gone (this is the latest log entry)";
        if ($ps_leftover{$o_ppid} && $ps_leftover{$o_ppid}{COMMAND} =~ /\bperl\b/) {
            # could check the logger process exists under, but Q'n'D
            $msg = "Otterlace remained (still running at time of diagnosis?)";
        }
        printf "%s:%6d: %s\n", $last_dt, $o_ppid, $msg;
        my @z_pid = grep { $zmap{$_}[1] eq $o_ppid } keys %zmap;
        z_infer_gone($last_dt, $o_ppid, @z_pid);
    }

    sub all_gone {
        z_infer_gone(END => undef, keys %zmap);
        my @other_zmap = grep { $_->{COMMAND} =~ /\bzmap\b/ }
          values %ps_leftover;
        warn Dump({ stray_zmaps => \@other_zmap }) if @other_zmap;
    }
}

sub by_pid {
    return $a <=> $b;
}

sub by_rev_mtime {
    return -M $b <=> -M $a;
}


sub main {
    readfiles("/nfs/users/nfs_m/mca/public_html/RT/Deepa-oom.RT395402/");

    @ARGV = sort by_rev_mtime @ARGV;

    my ($last_dt, $last_pid);
    while (<>) {
        my ($ppid) = $ARGV =~ m{otter/otterlace\.(\d+)-}
          or die "Cannot get Otterlace pid from fn=$ARGV";
        if (m{^ +\| (.*)}) {
            # continuation line
            next;
        }
        my ($dt, $msg_type, $msg) = m{^(\d{4}-\d{2}-\d{2} [0-9:]{8},\d+) ([^:]*): (.*)$}
          or die "Cannot get timestamp from $_";

        if ($msg =~ m{^Started zmap, pid (\d+)$}) {
            z_add($ppid, $1, $dt);

        } elsif ($msg =~ m{^Process (\d+) exited}) {
            z_gone($ppid, $1, $dt, $msg);

        } elsif ($msg =~ m{^=== } # stack trace
                 # does not tell pid.  RT#395448
                 # also does not mean the process has gone.  RT#272216

                 || $msg =~ m{^git HEAD:}
                 # tells the version

                 || $msg =~ m{^X Error of failed request:}
                 # vanished - probably otterlace
                ) {
            printf "%s:%6d: %s\n", $dt, $ppid, $msg;

        } elsif ($msg_type eq 'otter.main INFO' && $msg eq 'Exiting') {
            o_gone($dt, $ppid);

        } # else ignore cruft

        $last_pid = $ppid;
        $last_dt  = $dt;
    } continue {
        if (eof) {  # Not eof()!
            o_infer_gone($last_dt, $last_pid);
            close ARGV;
        }
    }

    all_gone();
}

main();
