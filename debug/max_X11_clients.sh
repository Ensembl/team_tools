#! /bin/sh

# See https://rt.sanger.ac.uk/Ticket/Display.html?id=226996
#     [deskpro20130 (Lenny) "Maximum number of clients reached" - X11 error]
#
# This script gathers relevant information.  It may leak private
# authentication cookies etc.

CODE="$( dirname "$0" )"
now="$( date +%FT%H%M%S )"
host="$( hostname -f )"
echo "* host=$host user=$USER now=$now DISPLAY=$DISPLAY -*- outline -*-
Code in $CODE

** xwininfo -root -children"
while ! xwininfo -root -children 2>&1; do
    sleep 2
    date >&2
    echo "  Cannot get an X11 connection
  Please close one application, but not Otterlace
" >&2
done
echo Dumping debug info >&2
echo "** xwininfo -root -tree -all"
xwininfo -root -tree -all 2>&1
echo "** show-x-sockets.py"
$CODE/show-x-sockets.py 2>&1
echo "** xrestop -b -m 1"
xrestop -b -m 1 2>&1
echo "** ps aux"
ps aux
echo "** Finished"
date
echo Finished >&2
