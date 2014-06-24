#! /software/bin/perl -T
#  ^^^^^^^^^ just a bodge for easy File::Slurp deps

use strict;
use warnings;
use File::Slurp 'read_dir';
use YAML qw( Dump LoadFile );
use Getopt::Long;

sub main {
    my $ok = 1;
    my ($dir, $days, $snaps);
    $ok &&= GetOptions('dir|d' => \$dir, 'a=i' => \$days, 'snapshot|S' => \$snaps);
    $ok &&= !@ARGV;
    $ok &&= ($dir xor $days);
    $ok &&= $days if $snaps;
    die "Syntax: $0 < -d | [ -S ] -a <days> >\n
  -a\tList files up to <days> old
  -d\tList the directories
  -S\tWhen listing files, look also in .snapshot/\n" unless $ok;

    my @dirs = logdirs();

    my @out;
    if ($days) {
        my @logs;
        foreach my $dir (@dirs) {
            my @recent = files_in(qr{/otterlace\..*\.log$}, $dir);
            push @logs, @recent;
            next unless $snaps;

            my $snapdir = "$dir/.snapshot";
            next unless -d $snapdir;
            my @weekly = sort { length($a) <=> length($b) or $a cmp $b }
              files_in(qr{/weekly\.}, $snapdir);
            my @more_logs = map { files_in(qr{/otterlace\..*\.log$}, $_) } @weekly;
            my %leaf # leaf => length
              = map {( leaf($_), -s $_ )} @recent;
            foreach my $log (@more_logs) {
                my $leaf = leaf($log);
                if (!defined $leaf{$log}) {
                    # new leafname
                    push @logs, $log;
                    $leaf{$leaf} = -s $log;
                } elsif ($leaf{$leaf} == -s $log) {
                    # same file, no action
                } else {
                    warn "$log might be different?  Ignored it anyway";
                }
            }
        }

        @out = grep { -M $_ <= $days } @logs;
    } else {
        @out = @dirs;
    }

    print map {"$_\n"} @out;
}


sub logdirs {
  my @u = LoadFile('users.yaml');
  my %u = eval {
    %{ $u[0]->{users} };
  } or die "Bad userlist";

  my @d = sort values %u;
  return @d;
}

sub files_in {
  my ($pat, $dir) = @_;
  return grep { $_ =~ $pat } map {"$dir/$_"} read_dir($dir);
}

sub leaf {
    my ($fn) = @_;
    die "leaf?! $fn" unless $fn =~ m{/([^/]+)$};
    return $1;
}

main();
