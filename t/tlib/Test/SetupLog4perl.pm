package Test::SetupLog4perl;

use strict;
use warnings;

use Log::Log4perl qw(:easy);

my $verbose = (
       $ENV{HARNESS_IS_VERBOSE} #    prove -v
    or defined $DB::single      # or running under perl debugger
    );
my $level = $verbose ? $DEBUG : $WARN;

Log::Log4perl->easy_init({ level => $level, file => 'stdout', layout => '# %-5p %c: %m%n' });

$level = $DB::single; # avoid 'used only once' warning on $DB::single

1;

