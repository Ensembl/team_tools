package Bio::EnsEMBL::Analysis::Programs;
use strict;
use warnings;

=head1 DESCRIPTION

Compile-time checking of executables' presence not needed during
syntax checking.

=head2 Problem solved

Some scripts and modules do

 use Bio::EnsEMBL::Analysis::Programs qw( this_prog );

The Perl module lives in core Ensembl and checks at compile-time that
C<this_prog> is available on C<PATH>.  It may not be, but for syntax
checking this is merely a distraction.

=head1 AUTHOR

Ana Code B<email> anacode@sanger.ac.uk

=cut

sub import {
    my ($pkg, @prog) = @_;
    warn "Program availability check '$_' skipped" foreach @prog;
    return ();
}

1;
