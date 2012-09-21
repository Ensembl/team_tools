
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


=head1 NAME

Perl::Critic::Policy::Anacode::ProhibitGiven - Do not use C<given()>.


=head1 DESCRIPTION

Perl's C<given> statement creates a lexical binding for C<$_> which
can lead to counterintuitive behaviour and bugs.  See "L<Use for()
instead of given()|http://www.effectiveperlprogramming.com/blog/1333>".

Use C<for()> (with no explicit loop variable) instead of C<given()>.
This creates a local binding for the global C<$_>.  This plays well
with standard Perl idioms such as iterators that locally bind C<$_>
and execute a block.  The C<when()> and C<default> statements work
inside a C<for()> statement just as they do inside C<given()>.


=head1 AUTHOR

Ana Code B<email> anacode@sanger.ac.uk
