
package Perl::Critic::Policy::Anacode::ProhibitRebless;

use strict;
use warnings;

use base qw( Perl::Critic::Policy );

use Perl::Critic::Utils qw( :severities );
use Try::Tiny;

sub supported_parameters { return (); }
sub default_severity { return $SEVERITY_MEDIUM; }
sub default_themes { return qw( maintenance anacode ); }
sub applies_to { return qw( PPI::Token::Word ) };

sub violates {
    my ($self, $element, $doc) = @_;
    return unless $element eq 'bless';

    # walk up
    my $sub = $element;
    while ($sub && ! try { $sub->isa('PPI::Statement::Sub') }) {
        $sub = $sub->parent;
    }
    my $name = $sub ? 'sub '.$sub->name : 'toplevel';

    return if $name eq 'sub new';

    return $self->violation(
        qq("bless" statement in $name),
        q(Call "bless" only in method "new", do not rebless.),
        $element);
}

1;


=head1 NAME

Perl::Critic::Policy::Anacode::ProhibitRebless - C<bless> only in C<new>


=head1 DESCRIPTION

Constructors do C<bless>ing.  Other methods calling C<bless> are
"reblessing".

This is considered (by Anacode and Ensembl Core) to be "clever", and
should be avoided; though finding a neat replacement can be tricky.

 package Foo;
 use base 'Bar';
 sub new {
   ...
   bless $self, $pkg;   # ok
   return $self;
 }
 
 sub _specialise {
   my ($self, $pkg) = @_;
   bless $self, $pkg;   # dubious
   return $self;
 }


 package main;
 my $f = Foo->new;
 bless $f, 'Bar';       # bad
 $f->specialise;        # better (dubiousness is at least delegated)


=head1 AUTHOR

Ana Code B<email> anacode@sanger.ac.uk
