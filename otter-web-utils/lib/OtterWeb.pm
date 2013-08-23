package OtterWeb;
use strict;
use warnings;

# This is a quick hack to get the passwords out of the source,
# and at the same time shrink the CGI scripts a little.
use DBI;
use base 'Exporter';

use CGI::Carp 'fatalsToBrowser';
use SangerPaths qw( otter-dev );
use Bio::Otter::Server::Config;

our @EXPORT_OK = qw( otterlive_dbh );


sub otterlive_dbh {
    my ($db) = @_;
    $db = $db ? ";database=$db" : "";

    my $dataset = Bio::Otter::Server::Config->SpeciesDat->dataset('human');
    my ($user, $pass) = @{ $dataset->params }{qw{ USER PASS }};

    return DBI->connect
      ("DBI:mysql:host=otterlive;port=3324;database=${db}",
       $user, $pass, { RaiseError => 1, AutoCommit => 0 } );
}

1;
