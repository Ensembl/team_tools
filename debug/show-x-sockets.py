#!/usr/bin/python

from subprocess import Popen, PIPE

client_sockets = []
match = 0

ns = Popen(["netstat", "-an", "--unix"], stdout=PIPE)
output = ns.communicate()[0]
for line in output.split('\n'):
	if line.find("X11-unix") != -1:
		match = 1
	elif match:
		match = 0
		inode = line.split()[6]
		client_sockets.append(inode)

lsof = Popen(["lsof", "-U", "+c0", "-w"], stdout=PIPE)
output = lsof.communicate()[0]
wantcol = 6 # Debian Etch
for line in output.split('\n'):
	if "DEVICE SIZE/OFF" in line:
		wantcol = 7 # Ubuntu Lucid
	try:
		inode = line.split()[wantcol]
		if inode in client_sockets:
			print line
	except:
		pass

# Subject: Re: [Hinxton #226996] deskpro20130 (Lenny) "Maximum number of clients reached" - X11 error
# From: Dave Holland via RT <syshelp@sanger.ac.uk>
# Date: Fri, 5 Aug 2011 14:52:38 +0100
