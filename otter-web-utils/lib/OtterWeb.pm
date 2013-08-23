package OtterWeb;
use strict;
use warnings;

# This is a quick hack to get the passwords out of the source,
# and at the same time shrink the CGI scripts a little.
use DBI;
use base 'Exporter';

our @EXPORT_OK = qw( otterlive_dbh );


sub otterlive_dbh {
    my ($db) = @_;
    $db = $db ? ";database=$db" : "";

    return DBI->connect
      ("DBI:mysql:host=otterlive;port=3324;database=${db}",
       "ottadmin", "wibble", { RaiseError => 1, AutoCommit => 0 } );
}

1;
