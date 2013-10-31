#! /software/bin/perl-5.12.2
use strict;
use warnings;

use YAML qw( Dump LoadFile );
use List::MoreUtils 'uniq';


sub main {
    my ($fn) = @_;
    my (%qy, @req);
    {
        my $info = LoadFile($fn);
        die "bad input $fn" unless eval { $info->{req} && $info->{qy} };
        %qy = %{ $info->{qy} };
        @req = @{ $info->{req} };
    }

    # Drop the happies
    while (my ($q, $v) = each %qy) {
        my $resp = $v->{resp};
        next if $v->{DIFF};
        if ((join ',', keys %{ $v->{orig} }) eq '200' && # original was OK
            (join ',', values %$resp) eq '2' &&          # fetches unanimous
            (keys %$resp)[0] =~ /^\S+ 200 \d+$/) {       # fetches OK
            delete $qy{$q};
        }
    }

    @req = grep { $qy{$_} } @req;
    @req = uniq(sort @req);

    print Dump({ req => \@req, qy => \%qy,
                 summary => { n_req => scalar @req,
                              n_qy => scalar keys %qy },
               });

    return 0;
}


exit main(@ARGV);
