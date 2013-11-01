#! /software/bin/perl-5.12.2

use File::Slurp qw( write_file slurp );


=head1 DESRIPTION

 foreach @ARGV
   sort all attrs on each line
   sort all lines
   produce sorted file with different filename


=head1 CAVEAT

(10:16:46) jgrg: In theory GFF line order ought not to be important,
but scripts which construct transcripts may assume that all the
components of a transcript are contiguous.

=cut


sub main {
    foreach my $fn (@ARGV) {
        my $out_fn = "${fn}_canon.gff";
        my @ln = slurp($fn);
        die "not gff2? $fn:$ln[0]" unless $ln[0] =~ /^##gff-version (2\.0|2)$/;
        # 3... untested

        # Safety
        my $flushers;
        foreach (@ln) {
            die "##FASTA not handled in $fn" if /^##FASTA|^>/;
            $flushers ++ if /^###/;
        }
        if ($flushers) {
            @ln = grep { $_ ne "###\n" } @ln;
            warn "Removed ### feature flush from $out_fn";
        }

        # Split & sort attrs
        my @ln_field = map {[ split /\t/ ]} @ln;
        foreach my $f (@ln_field) {
            chomp $f->[-1];
            my @a = split /\s*;\s*/, $f->[8];
            $f->[8] = join ';', sort @a if defined $f->[8];
        }

        # Sort lines & stitch
        my $out = join '',
          map { (join "\t", @$_)."\n" }
            sort gffly @ln_field;

        write_file($out_fn, { atomic => 1}, $out);
    }
    return 0;
}

sub gffly {
    return 0 if 1 == @$a && 1 == @$b; # stable for headers
    return ((1 == @$b) <=> (1 == @$a) || # is header?
            $a->[3] <=> $b->[3] || # start
            $a->[4] <=> $b->[4] || # end
            $a->[6] cmp $b->[6] || # strand
            $a->[8] cmp $b->[8]);  # attrs
}


exit main();
