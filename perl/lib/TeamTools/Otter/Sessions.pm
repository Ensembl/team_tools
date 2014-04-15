package TeamTools::Otter::Sessions;

use strict;
use warnings;

use Carp;
use List::MoreUtils qw{ uniq };
use Readonly;

Readonly my $SESSION_STEM    => '/var/tmp/lace_';
Readonly my $SESSION_GLOB    => "${SESSION_STEM}*";
Readonly my $SESSION_SEARCH  => qr{($SESSION_STEM\d+\.\w+\.\d+\.\d+)}o;
Readonly my $SESSION_DONE    => '.done';
Readonly my $SESSION_INDEX   => qr{\.(\d+)(?:\.done)?$};


sub new {
    my ($pkg, %opt) = @_;
    my $self = {};
    while (my ($key, $value) = each %opt) {
        if ($value) {
            $key =~ s/^--?//;   # strip - or -- from --options
            $self->{$key} = $value;
        }
    }
    return bless $self, $pkg;
}

sub session_path {
    my ($self, @args) = @_;
    ($self->{'session_path'}) = @args if @args;

    my $session_path = $self->{'session_path'};
    unless ($session_path) {
        $session_path = $self->{'session_path'} = $self->_build_session_path;
    }

    if ($session_path) {
        -d $session_path or croak "Cannot read session directory '$session_path': $!";
    }

    return $session_path;
}

sub _build_session_path {
    my ($self) = @_;

    my $log_search = $self->log_search;
    return unless $log_search;

    my $index  = $self->index;
    my $sessions = $self->log_sessions;

    unless ($index) {
        # Look for a single match
        my @paths = keys %$sessions;
        return unless scalar(@paths) == 1;
        my $path = $paths[0];
        return unless $sessions->{$path}->{status} =~ m/^(active|finished)$/;
        return $path;
    }

    while (my ($path, $details) = each %$sessions) {
        return $path if $details->{status} eq 'active'   and $details->{index} == $index;
        return $path if $details->{status} eq 'finished' and $details->{index} == $index;
    }
    return;
}

sub log_search {
    my ($self, @args) = @_;
    ($self->{'log_search'}) = @args if @args;
    my $log_search = $self->{'log_search'};
    return $log_search;
}

sub index {
    my ($self, @args) = @_;
    ($self->{'index'}) = @args if @args;
    my $index = $self->{'index'};
    return $index;
}

sub all {
    my ($self, @args) = @_;
    ($self->{'all'}) = @args if @args;
    my $all = $self->{'all'};
    return $all;
}

sub sessions {
    my ($self) = @_;
    return $self->log_sessions if $self->log_search;
    return $self->_classify_sessions($self->session_path) if $self->session_path;
    return $self->_classify_sessions($self->glob_sessions);
}

sub glob_sessions {
    my ($self) = @_;
    my @sessions;
    foreach my $s (glob($SESSION_GLOB)) {
        unless ($s =~ m/$SESSION_SEARCH/) {
            warn "Skipping: $s\n";
            next;
        }
        push @sessions, $s;
    }
    return @sessions;
}

sub _classify_sessions {
    my ($self, @sessions) = @_;
    my %results;

    foreach my $path (@sessions) {

        my $session;
        $session->{status} = 'NOT FOUND';
        ($session->{index}) = ($path =~ m/${SESSION_INDEX}/);

        my $done = "${path}${SESSION_DONE}";

        if ($path =~ m/${SESSION_DONE}$/o) {
            # Just check the done path
            $done = $path;
            $path = undef;
        }

        if ($path and -d $path) {
            $session->{status} = 'active';
        }
        elsif (-d $done ) {
            $path = $done;
            $session->{status} = 'finished';
        }

        $results{$path} = $session;
    }
    return \%results;
}

sub log_session_matches {
    my ($self) = @_;
    my $log_search = $self->log_search;
    open my $fh, '<', $log_search or croak "Cannot open '$log_search': $!";
    my @matches;
    while (<$fh>) {
        my ($session) = m/$SESSION_SEARCH/;
        push @matches, $session if $session;
    }
    return uniq @matches;
}

sub log_sessions {
    my ($self) = @_;
    return $self->_classify_sessions($self->log_session_matches);
}

1;
