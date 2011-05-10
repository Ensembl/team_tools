
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
    'OtterDefs.pm', # reads a file that is not available locally
    ;

# Declare some variables that are normally declared by the modules
# that we just faked.

package Bio::Otter::ServerSide;
use vars qw( $OTTER_SPECIES ); # declared by the real OtterDefs.pm

1;
