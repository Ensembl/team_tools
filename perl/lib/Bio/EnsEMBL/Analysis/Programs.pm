package Bio::EnsEMBL::Analysis::Programs;
use strict;
use warnings;

=head1 DESCRIPTION

Compile-time checking of executables' presence not needed during
syntax checking.

=head1 AUTHOR

Ana Code B<email> anacode@sanger.ac.uk

=cut

sub import {
    my ($pkg, @prog) = @_;
    warn "Program availability check '$_' skipped" foreach @prog;
    return ();
}

1;
