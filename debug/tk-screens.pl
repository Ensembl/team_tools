#! /software/bin/perl-5.12.2
use strict;
use warnings;
use Tk;
use Try::Tiny;

sub main {
    my $mw = MainWindow->new;

    my @screen = probe_screens($mw);
    print("Screens are\n", map {"  $_\n"} @screen);
}

main();


# Tk advertises no method to discover other screens, but we can ask
sub probe_screens {
    my ($mw) = @_;
    my $current = $mw->screen;
    my ($display, $screen) = $current =~ m{^(:\d+)\.(\d+)$};

    if (!defined $display) {
        warn "Failed to parse screen name '$current', assuming one screen";
        return ($current);
    } else {
        my @dpy;
        for (my $i=0; $i <= 255; $i++) {
            last unless
              try {
                  my $name = "$display.$i";
                  my $tl = $mw->Toplevel(-screen => $name);
                  # still here - success
                  $dpy[$i] = $name;
              } catch {
                  warn "Unexpected error in probe_screens: $_"
                    unless m{couldn't connect to display|bad screen number};
                  0;
              };
        }

        # Rotate them to start from 1+$screen,
        # so that $dpy[0] is the "next" one
        push @dpy, splice(@dpy, 0, 1+$screen);
        return @dpy;
    }
}

