package TeamTools::Otter::Sessions;

use strict;
use warnings;

use Carp;
use List::MoreUtils qw{ uniq };
use Readonly;

Readonly my $SESSION_STEM    => '/var/tmp/otter_';
Readonly my $SESSION_GLOB    => "${SESSION_STEM}*/v*";
Readonly my $SESSION_SEARCH  => qr{($SESSION_STEM\w+/v\d+\.\d+\.\d+)}o;
Readonly my $SESSION_DONE    => '.done';
Readonly my $SESSION_INDEX   => qr{\.(\d+)(?:\.done)?$};
Readonly my $SESSION_DB      => 'otter.sqlite';


sub new {
    my ($pkg, %opt) = @_;
    my $self = {};
    while (my ($key, $value) = each %opt) {
        if (defined $value) {
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
        -f "${session_path}/${SESSION_DB}" or croak "'$session_path' does not look like a session directory";
    }

    return $session_path;
}

sub _build_session_path {
    my ($self) = @_;

    return $self->_path_from_log_search if $self->log_search;
    return $self->_nth_path             if defined $self->nth;
    return;
}

sub _path_from_log_search {
    my ($self) = @_;

    my $index  = $self->index;
    my @sessions = $self->log_sessions;

    unless ($index) {
        # Look for a single match
        return unless scalar(@sessions) == 1;
        return unless $sessions[0]->{status} =~ m/^(active|finished)$/;
        return $sessions[0]->{path};
    }

    foreach my $s (@sessions) {
        return $s->{path} if $s->{index} == $index and $s->{status} =~ m/^(active|finished)$/;
    }
    return;
}

sub _nth_path {
    my ($self) = @_;
    my @sessions = $self->_classify_sessions($self->glob_sessions);
    my $n = $self->nth;
    return unless $sessions[$n];
    return $sessions[$n]->{path};
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

sub nth {
    my ($self, @args) = @_;
    ($self->{'nth'}) = @args if @args;
    my $nth = $self->{'nth'};
    return $nth;
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
    my @results;

    my $log_search = $self->log_search;
    my $all        = ($self->all or $log_search);

    foreach my $path (@sessions) {

        my $session;
        $session->{status} = 'NOT FOUND';
        ($session->{index}) = ($path =~ m/${SESSION_INDEX}/);

        my $done = "${path}${SESSION_DONE}";

        if ($path =~ m/${SESSION_DONE}$/o) {
            # Just check the done path
            next unless $all;
            $done = $path;
            $path = undef;
        }

        if ($path and -d $path) {
            $session->{status} = 'active';
        }
        elsif ($all and -d $done) {
            $path = $done;
            $session->{status} = 'finished';
        }
        $session->{path}   = $path;

        push @results, $session;
    }
    @results = sort { -M $a->{path} <=> -M $b->{path} } @results unless $log_search; # newest first
    return @results;
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
