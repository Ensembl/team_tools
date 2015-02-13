package Test::CriticModule;

use strict;
use warnings;

use base 'Exporter';
our @EXPORT = qw(critic_module_ok); ## no critic (ProhibitAutomaticExportation)

use File::Spec;

BEGIN {
    # Ensure team_tools/perl/lib is in INC before loading perl critic
    my $tt;
    if ($tt = $ENV{ANACODE_TEAM_TOOLS} and -d $tt) {
        unshift(@INC, File::Spec->catfile($tt, 'perl', 'lib'));
    }
}

use Test::Builder;
use Test::Perl::Critic;

sub import {
    my ($self, %args) = @_;

    my ($tt_rc, %criticrc);
    if ($tt_rc = $ENV{ANACODE_PERLCRITICRC} and -r $tt_rc) {
        Test::Builder->new->note("Using perlcriticrc: $tt_rc");
        $criticrc{'-profile'} = $tt_rc;
    }

    Test::Perl::Critic->import(%criticrc, %args);
    $self->export_to_level( 1, $self, qw(critic_module_ok) );
    return 1;
}

sub critic_module_ok {
    my ($module, $test_name, @args) = @_;

  SKIP: {
    if ($ENV{OTTER_TESTS_SKIP_CRITIC}) {
        my $tb = Test::Builder->new;
        $tb->skip("Skipping critic_module_ok($module)");
        return;
    }

    my @mod_dir = split(/::/, $module);
    my $mod_file = pop(@mod_dir) . ".pm";
    my $mod_rel_path = File::Spec->catfile(@mod_dir, $mod_file);
    my $mod_path = $INC{$mod_rel_path};

    unless ($mod_path) {
        my $tb = Test::Builder->new;
        $tb->ok(0, $test_name || "Test::CriticModule for \"$module\"");
        $tb->diag("Cannot find '$module' in %INC - did you forget to 'use_ok($module)' first?");
        return;
    }

    return critic_ok($mod_path, $test_name, @args);

  } # SKIP
}

1;

__END__

=head1 NAME

Test::CriticModule - Run Perl::Critic as a test against a module

=head1 SYNOPSIS

  use lib "${ENV{ANACODE_TEAM_TOOLS}}/t/tlib";
  use Test::CriticModule;

  my $module = 'Bio::Otter::BAM';
  BEGIN { use_ok($module); }      # ensures module's path is in %INC

  critic_module_ok($module);

=head1 DESCRIPTION

Test::CriticModule is a convenience wrapper for L<Test::Perl::Critic> which
finds an already-I<require>'d module and runs L<Perl::Critic> against it.

The C<ANACODE_PERLCRITCRC> environment variable is detected and passed
through as the Perl::Critic profile if set to a readable file.

Arguments to C<use Test::CriticModule> are passed through to
Test::Perl::Critic.

=head1 EXPORTS

  critic_module_ok()

=head1 SEE ALSO

L<Test::Perl::Critic>

=head1 AUTHOR

Ana Code B<email> anacode@sanger.ac.uk

=cut

# EOF
