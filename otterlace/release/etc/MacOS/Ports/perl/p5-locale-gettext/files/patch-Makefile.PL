--- gettext-1.05/Makefile.PL.orig	2012-08-14 15:48:43.000000000 +0100
+++ gettext-1.05/Makefile.PL	2012-08-14 15:22:54.000000000 +0100
@@ -7,6 +7,13 @@
 } else {
 	$cc = $Config{'cc'};
 }
+
+my $flags = '';
+foreach my $e ( qw{ CPPFLAGS CFLAGS LDFLAGS } ) {
+    $flags .= $ENV{$e} . ' ' if $ENV{$e};
+}
+$cc .= ' ' . $flags if $flags;
+
 my $libs = '';
 
 unless (conftest("char *x = gettext(\"foo\");", "gettext", 0)) {
@@ -35,6 +42,8 @@
     NAME => "Locale::gettext",
     LIBS => ($libs eq '') ? [] : [$libs],
     VERSION_FROM => 'gettext.pm', 
+    CC => $cc,
+    LD => $cc,
 );
 
 sub conftest {
@@ -50,7 +59,7 @@
 	close TEST;
 	open(SAVE, ">&STDERR");
 	open(STDERR, ">/dev/null");
-	system($cc . " -o conftest " . $libs . " conftest.c");
+	system($cc . " -o conftest " . "conftest.c " . $libs);
 	my $exitstatus = $?;
 	open(STDERR, ">&SAVE");
 	if ($exitstatus != 0) {
