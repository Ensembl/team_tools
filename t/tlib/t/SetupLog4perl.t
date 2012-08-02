#!/usr/bin/env perl

use strict;
use warnings;

use Test::More tests => 1;

use Log::Log4perl;              # no setup

my $module;

BEGIN {
      $module = 'Test::SetupLog4perl';
      use_ok($module);
}

# Could be much more clever here, but just emit a warn and a debug for visual inspection.

my $logger = Log::Log4perl->get_logger;
$logger->warn('A warning');
$logger->debug('A debug message');

1;

# Local Variables:
# mode: perl
# End:

# EOF
