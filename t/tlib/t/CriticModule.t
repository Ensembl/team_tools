#!/usr/bin/env perl

use strict;
use warnings;

use Test::More tests => 2;

my $module;

BEGIN {
      $module = 'Test::CriticModule';
      use_ok($module, -severity => 4);
}

critic_module_ok($module);

1;

# Local Variables:
# mode: perl
# End:

# EOF
