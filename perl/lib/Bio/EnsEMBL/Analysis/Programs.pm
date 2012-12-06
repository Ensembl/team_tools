package Bio::EnsEMBL::Analysis::Programs;
use strict;
use warnings;

=head1 DESCRIPTION

Compile-time checking of executables' presence not needed during
syntax checking.

=cut

sub import {
    my ($pkg, @prog) = @_;
    warn "Program availability check '$_' skipped" foreach @prog;
    return ();
}

1;
