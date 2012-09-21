
package Perl::Critic::Policy::Anacode::ProhibitEval;

use strict;
use warnings;

use base qw( Perl::Critic::Policy );

use Perl::Critic::Utils qw( :severities );

sub supported_parameters { return (); }
sub default_severity { return $SEVERITY_MEDIUM; }
sub default_themes { return qw( bugs anacode ); }
sub applies_to { return qw( PPI::Token::Word ) };

sub violates {
    my ($self, $element, undef) = @_;
    return unless $element eq 'eval';
    return $self->violation(
        q("eval" statement),
        q(Use "try" instead of "eval".),
        $element);
}

1;


=head1 NAME

Perl::Critic::Policy::Anacode::ProhibitEval - Do not use C<eval>.


=head1 DESCRIPTION

Perl's C<eval> statement is awkward and hard to use correctly.
Instead declare "use Try::Tiny" and use C<try>.


=head1 AUTHOR

Ana Code B<email> anacode@sanger.ac.uk
