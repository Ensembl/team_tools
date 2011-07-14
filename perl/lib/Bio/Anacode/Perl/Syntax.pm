
# This module fakes some other Perl modules that are not in the
# developers' environment but exist when looked for in their proper
# context.  This fixes some false failures when syntax checking Perl.

## no critic(Modules::ProhibitMultiplePackages)

package main;

use strict;
use warnings;

# Fake include some modules.
$INC{$_}++ for

    # exist on the web server
    qw(
    SangerPaths.pm
    SangerWeb.pm
    ),

    # exist on cbi4
    qw(
    WrapDBI.pm
    ),

    ;

1;
