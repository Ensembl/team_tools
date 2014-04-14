package TeamTools::Otter::Logs;

use strict;
use warnings;

use Carp;
use English;
use Readonly;

Readonly my $DOT_OTTER => '.otter';
Readonly my $OTTERLOG_SEARCH => qr{^otterlace\..*\.log$};

sub new {
    my ($pkg, @opts) = @_;
    return bless { @opts }, $pkg;
}

# Defaults to current real user
sub user {
    my ($self, @args) = @_;
    my $user = $self->{'user'};
    if (@args) {
        ($user) = ($self->{'user'}) = @args;
        croak "No such user '$user'" unless getpwnam($user);
    } elsif (not defined $user) {
        $user = $self->{'user'} = getpwuid($REAL_USER_ID); # default
        croak "No such user '$user'" unless defined $user;
    }
    return $user;
}

sub nth {
    my ($self, @args) = @_;
    ($self->{'nth'}) = @args if @args;
    my $nth = $self->{'nth'};
    return $nth || 0;
}

sub home {
    my ($self, @args) = @_;
    ($self->{'home'}) = @args if @args;
    my $home = $self->{'home'};
    unless ($home or @args) {
        my $user = $self->user;
        $self->{'home'} = $home = (getpwnam($user))[7];
        -d $home or croak "Cannot read homedir '$home' for user '$user': $!";
    }
    return $home;
}

sub log_dir {
    my ($self, @args) = @_;
    ($self->{'log_dir'}) = @args if @args;
    my $log_dir = $self->{'log_dir'};
    unless ($log_dir or @args) {
        my $home = $self->home;
        $self->{'log_dir'} = $log_dir = "$home/$DOT_OTTER";
    }
    croak "Cannot open log_dir '$log_dir'" unless $log_dir and -d $log_dir;
    return $log_dir;
}

sub logs {
    my ($self) = @_;

    my $log_dir = $self->log_dir;
    opendir my $ld, $log_dir or croak "Cannot opendir '$log_dir': $!";

    my @logs =
        sort { -M $a <=> -M $b }
        map { "$log_dir/$_" }
        grep { m/$OTTERLOG_SEARCH/ }
        readdir($ld);

    return @logs;
}

sub log_file {
    my ($self) = @_;

    my $N = my @log = $self->logs;

    my $log_dir = $self->log_dir;
    my $many = "Logdir '$log_dir' has $N logfiles\n";

    my $nth = $self->nth;
    warn  $many if not $nth;
    croak $many if not $log[$nth];

    return $log[$nth];
}

1;
