package Log::Vulture;
use Moose;
#use Log::Chunk;

has fname => (is => 'ro', required => 1,
	      documentation => 'Input filename; construct with this');

has fh => (is => 'ro', lazy_build => 1);

has out => (is => 'ro', isa => 'ArrayRef', default => sub { [] },
	    documentation => 'Collected output');

has about => (is => 'ro', isa => 'HashRef', default => sub { {} }, lazy => 1,
	      documentation => 'Info about the Otterlace running');


sub _build_fh {
  my ($self) = @_;
  my $fn = $self->fname;
  if (open my $fh, '<', $fn) {
    return $fh;
  } else {
    die "$fn failed: $!";
  }
}

sub output {
  my ($self) = @_;
  my $fh = $self->fh;

  my $out = $self->out;
  my $chunk = 0; # index into out, where we append
  my $append = sub {
    $$out[$chunk] = '' unless defined $$out[$chunk];
    $$out[$chunk] .= join '', @_;
    return ();
  };
#  my $put = sub {
#    my ($type, @ln) = @_;
#    my $ch = Log::Chunk->new;
#    $ch->set(0, $type);
#    $ch->set(1, $.);
#    $ch->set(2, join '', @ln);
#    $$out[$chunk] = $ch;
#    ++$chunk;
#  };

  my @context_pre;
  my $context_post = 0;

  my $tsre = qr{^\w{3} \w{3} [0-9 ]\d [0-9]{2}:[0-9]{2}:[0-9]{2} \d{4}  };

  while (<$fh>) {
    # Data to extract, but forget the raw line
    my %extr;
    if (/${tsre}git HEAD: (\S+)$/) {
      $extr{otterlace_version} = $1;
    } elsif (m{ at (.*?)/ensembl-otter/modules/\S+ line \d+\.$}) {
      $extr{otter_home} = $1;
    } elsif (m{${tsre}GET  http\S+/lock_region\?(?:\S+&)?hostname=([^&;=]+)(?:&|$)}) {
      $extr{hostname} = $1;
    } elsif (m{${tsre}=== signal handler .* version = (\S+) ===$}) {
      $extr{zmap_version} = $1;
    }
    while (my ($k, $v) = each %extr) {
      $self->about->{$k} ||= [ $v, $. ];
    }

    if (m{${tsre}Express fetch for '(.*?_\d+-\d+)' took }){
      push @{ $self->about->{regions} }, [ $1, $. ];
    }


    # Lines to keep
    my $keep;
    if (/\bassertion failed\b/) {
      $keep = 'zmap_assert';
    } elsif (/${tsre}=== signal handler/ .. # XXX: evil flipflop
	     /${tsre}Process \d+ exited \d+$/) {
      $keep = 'zmap_stacktrace';
    } # else probably ignore

    if ($keep) {
#      $put->(context => @context_pre) if @context_pre;
#      $put->($keep => $_);
      $append->(@context_pre, $_);
      $context_post = 5;
      @context_pre = ();
    } elsif ($context_post > 0) {
      $append->($_);
      $context_post --;
      $chunk++ if $context_post == 0; # separate from next
    } else {
      push @context_pre, $_;
      shift @context_pre if @context_pre > 5;
    }

  }
  # EOF
  $self->clear_fh;

  return $self;
}


__PACKAGE__->meta->make_immutable;
no Moose;
