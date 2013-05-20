
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

# Bring in stubs.
# They are kept separate because most other code does not want to
# accidentally load them.
require lib;
my $dir = __FILE__;
my $subdir = 'stubs.syntax_check';
if ($dir =~ s{/lib/Bio/Anacode/Perl/Syntax\.pm$}{} &&
    -d "$dir/$subdir") {
    lib->import("$dir/$subdir");
} else {
    die "Cannot find my $subdir relative to $dir";
}

1;
