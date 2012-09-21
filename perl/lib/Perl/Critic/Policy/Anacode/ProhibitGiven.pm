
package Perl::Critic::Policy::Anacode::ProhibitGiven;

use strict;
use warnings;

use base qw( Perl::Critic::Policy );

use Perl::Critic::Utils qw( :severities );

sub supported_parameters { return (); }
sub default_severity { return $SEVERITY_MEDIUM; }
sub default_themes { return qw( bugs anacode ); }
sub applies_to { return qw( PPI::Statement::Given ) };

sub violates {
    my ($self, $element, undef) = @_;
    return $self->violation(
        q("given" statement),
        q("given" statements create strange behaviour and bugs.),
        $element);
}

1;

=head1 AUTHOR

Ana Code B<email> anacode@sanger.ac.uk
