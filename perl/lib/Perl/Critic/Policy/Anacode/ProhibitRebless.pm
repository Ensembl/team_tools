
package Perl::Critic::Policy::Anacode::ProhibitRebless;

use strict;
use warnings;

use base qw( Perl::Critic::Policy );

use Perl::Critic::Utils qw( :severities );
use Try::Tiny;

sub supported_parameters {
    return (
            {
             name => 'method',
             description => 'An anchored regexp string matching permitted constructor names.',
             default_string => '^sub (new|TIEHANDLE)$',
             behavior => 'string',
            },
            {
             name => 'excepted_filename',
             description => 'An anchored regexp string matching filenames which are permitted to ignore this policy.',
             default_string => '', # no exceptions
             behavior => 'string',
            },
           );
}

sub filename_re {
    my ($self) = @_;
    return $self->_regexpify('_excepted_filename');
}

sub method_re {
    my ($self) = @_;
    return $self->_regexpify('_method');
}

sub _regexpify {
    my ($self, $parm) = @_;
    if (!defined $self->{$parm}) {
        return undef; # blank, no exceptions
    } elsif (!ref($self->{$parm})) {
        my ($k, $v) = ($parm, $self->{$parm});
        $k =~ s/^_//;
        if ($v eq '') {
            # blank = no exceptions
            $self->{$parm} = undef;
            return undef;
        } else {
            die "Regexp string parameter $k='$v' lacks ^...\$ anchors"
              unless $v =~ /\^|\\A/ && $v =~ /\$|\\Z|\\z/;
            # the test isn't rigorous, but should prevent some mistakes
            $self->{$parm} = qr{$v};
        }
    }
    return $self->{$parm};
}

sub default_severity { return $SEVERITY_MEDIUM; }
sub default_themes { return qw( maintenance anacode ); }
sub applies_to { return qw( PPI::Token::Word ) };

sub violates {
    my ($self, $element, $doc) = @_;
    return unless $element eq 'bless';

    my $fn_re = $self->filename_re;
    return if defined $fn_re && $element->logical_filename =~ $fn_re;

    # walk up
    my $sub = $element;
    while ($sub && ! try { $sub->isa('PPI::Statement::Sub') }) {
        $sub = $sub->parent;
    }
    my $name = $sub ? 'sub '.$sub->name : 'toplevel';

    my $meth_re = $self->method_re;
    return if $name =~ $meth_re;

    return $self->violation(
        qq("bless" statement in $name),
        qq(Call "bless" only in a constructor method =~ $meth_re, do not rebless.),
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


=head1 EXCEPTIONS

There are inevitably exceptions to the rule; if they still have a code
smell, it isn't this one.

=over 4

=item * Populating structures for automated testing code

=item * Composite C<new> methods

=item * Multiple constructors e.g. C<new_from_wibble>

=back

=head1 CONFIGURATION

This Policy takes configuration to relax the restrictions, allowing
extra names of constructor methods and names of files.

(It's documented in L</supported_parameters> but I don't see how that
comes out in the user interface.)

=head1 AUTHOR

Ana Code B<email> anacode@sanger.ac.uk

=head1 AFFILIATION

This policy is part of Anacode's internal team_tools.
