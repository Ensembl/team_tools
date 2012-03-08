#! /bin/bash

host=$1

if [ "$host" = '--all' ]; then
    echo "HEADER,hostname,mem kB,expect user,seen users recent first"
    perl -ne 'print "$1\n" if /^  (deskpro-.*?):/' ~/.ssh/ssh-config.yaml | \
        xargs -n1 $0
elif [[ "$host" =~ ^--show ]]; then
    shift
    perl -e '
 use Sys::Hostname qw( hostname );
 use File::Slurp qw( slurp );
 use List::MoreUtils qw( uniq );
 my $txt = slurp("/proc/meminfo");
 my $mem = $txt =~ /^MemTotal:\s+(\d+) kB$/m ? $1 : "?";
 my $user = shift;
 $user =~ s/^deskpro-//;
 my @login = (qx( w ), qx( last ), qx( cp /var/log/wtmp.1.gz /tmp && gunzip -f /tmp/wtmp.1.gz && last -f /tmp/wtmp.1 && rm -f /tmp/wtmp.1 ));
 @login = map { m{^(\w+)\s+[pt]t[sy]/?\d+} ? ($1) : () } @login;
 printf("DATA%s,%s,%s,%s,%s\n", ($login[0] eq $user ? "" : " - CHECK"),
        hostname(), $mem, $user, join " ", uniq(@login));
' "$@"
elif [ -n "$host" ] && ! [[ "$host" =~ ^- ]]; then
    ssh $host $0 --show $host 2>&1 < /dev/null | \
        perl -ne '
 BEGIN { $|=1 }
 if (/^DATA/ || /^Offending key/) {
   # leave it over the left
   print;
 } else {
   # warn it over there
   print STDERR "    $_";
 }
'
else
    echo b0rk >&2
    exit 1
fi
