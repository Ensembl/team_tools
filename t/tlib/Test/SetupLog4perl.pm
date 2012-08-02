package Test::SetupLog4perl;

use strict;
use warnings;

use Log::Log4perl qw(:easy);

my $verbose = (
       $ENV{HARNESS_IS_VERBOSE} #    prove -v
    or defined $DB::single      # or running under perl debugger
    );
my $level = $verbose ? $DEBUG : $WARN;

Log::Log4perl->easy_init({ level => $level, file => 'stdout', layout => '# %-5p %c: %m%n' });

$level = $DB::single; # avoid 'used only once' warning on $DB::single

1;

__END__

=head1 NAME

Test::SetupLog4perl - Provide sensible Log4perl setup for testing

=head1 SYNOPSIS

  use lib "${ENV{ANACODE_TEAM_TOOLS}}/t/tlib";
  use Test::SetupLog4perl;

  my $module = 'Bio::Otter::GappedAlignment'; # uses Log::Log4perl
  BEGIN { use_ok($module); }

  # ...tests proceed on module which may emit debug info via Log4perl

=head1 DESCRIPTION

Test::SetupLog4perl makes an initialisation call to L<Log::Log4perl>
to set up sensible TAP-compatible output for any diagnostics
produced by the code under test which are routed via Log4perl.

Advantages:

=over

=item *
Stops Log::Log4perl complaining about lack of initialisation.

=item *
Messages are sent to C<stdout> prefixed with a '#' so that they appear
in verbose test output but don't interfere with TAP parsing.

=item *
The log level is set to I<DEBUG> if running under C<prove -v> or under
the perl debugger, and to I<WARN> otherwise.

=back

=head1 SEE ALSO

L<Log::Log4perl>

=head1 AUTHOR

Ana Code B<email> anacode@sanger.ac.uk

=cut

# EOF
