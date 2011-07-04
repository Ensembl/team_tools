
# This module fakes a few other Perl modules that are not found or do
# not work in the developers' environment but nevertheless work in
# their proper context.  This fixes some false negatives when syntax
# checking Perl.

## no critic(Modules::ProhibitMultiplePackages)

package main;

use strict;
use warnings;

# Fake include some modules.
$INC{$_}++ for

    # only on the web server
    qw(
    SangerPaths.pm
    SangerWeb.pm
    Bio/Das/Lite.pm
    ),

    'WrapDBI.pm', # only exists on cbi

    ;

1;
