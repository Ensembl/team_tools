
package Perl::Critic::Policy::Anacode::ProhibitAutoderef;

use strict;
use warnings;

use base qw( Perl::Critic::Policy );

use Perl::Critic::Utils qw( :severities :ppi );

sub supported_parameters { return (); }
sub default_severity { return $SEVERITY_HIGH; }
sub default_themes { return qw( bugs anacode ); }
sub applies_to { return qw( PPI::Token::Word ) };

my %AUTODEREF;
@AUTODEREF{qw{ push unshift pop shift splice keys values each }} =
  __qrify(qw(  @    @       @   @     @      %@   %@     %@   ));
### From https://metacpan.org/module/RJBS/perl-5.18.1/pod/perl5140delta.pod#Array-and-hash-container-functions-accept-references
#
# |----------------------------+---------------------------|
# | Traditional syntax         | Terse syntax              |
# |----------------------------+---------------------------|
# | push @$arrayref, @stuff    | push $arrayref, @stuff    |
# | unshift @$arrayref, @stuff | unshift $arrayref, @stuff |
# | pop @$arrayref             | pop $arrayref             |
# | shift @$arrayref           | shift $arrayref           |
# | splice @$arrayref, 0, 2    | splice $arrayref, 0, 2    |
# | keys %$hashref             | keys $hashref             |
# | keys @$arrayref            | keys $arrayref            |
# | values %$hashref           | values $hashref           |
# | values @$arrayref          | values $arrayref          |
# | ($k,$v) = each %$hashref   | ($k,$v) = each $hashref   |
# | ($k,$v) = each @$arrayref  | ($k,$v) = each $arrayref  |
# |----------------------------+---------------------------|

my %USE_INSTEAD;
@USE_INSTEAD{__qrify(qw( @ %@ % ))} = ('@{...}', '@{...} or %{...}', '%{...}');


sub violates {
    my ($self, $element, undef) = @_;
    return unless
      my $want_sigil = $AUTODEREF{$element};

    my $arg = first_arg($element);
    my $more_arg = '';
    if ($arg->class eq 'PPI::Token::Word') {
        # bad
    } elsif ($arg->class eq 'PPI::Token::Cast') {
        # @{ ... } or similar
        my $cast = $arg->content;
        return if $cast =~ $want_sigil;
        # Need to check this for @{ blah() }[1]  ...too weird to be real?

    } elsif ($arg->class eq 'PPI::Token::Symbol' &&
             $arg->raw_type =~ $want_sigil) {
        # @foo or similar.  Might be OK, unless it is a @weird[1]
        my $more = $arg->snext_sibling;
        return
          unless $more->class eq 'PPI::Structure::Subscript';
        $more_arg = $more->content;
    } # else not OK

    return $self->violation
      (qq{Auto-dereference of scalar in '$element $arg$more_arg...'},
       qq{Use '$element $USE_INSTEAD{$want_sigil}, ...'.},
       $element);
}


sub __qrify {
    my (@txt) = @_;
    my @re = map { qr{^[$_]$} } @txt;
    return @re;
}

1;


=head1 NAME

Perl::Critic::Policy::Anacode::ProhibitAutoderef - Avoid C<push $ary_ref> et al.

=head1 DESCRIPTION

Following
 http://modernperlbooks.com/mt/2012/03/inadvertent-inconsistencies-aggregate-autoderef-in-514.html
 http://modernperlbooks.com/mt/2012/03/inadvertent-inconsistencies-each-versus-autoderef.html

we avoid using Perl 5.14's new ability to automatically dereference
scalars as lists or hashes.

=head1 AUTHOR

Ana Code B<email> anacode@sanger.ac.uk
