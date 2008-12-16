#! /usr/bin/env perl

use CGI;

header();
foreach $lsLine (`ls -lt WebWiki/*.html`) {
    print_file($lsLine);
}
footer();

sub print_file {
    my($lsLine) = @_;
    my($file);

    $lsLine =~ /WebWiki\/(.*)/;
    $file = $1;

    print "<a href='WebWiki/$file'>" . pageName($file) . "</a><br/>\n";
}

# Given an HTML file name, return the readable page name.
sub pageName {
    my($pageName) = @_;

    # This is faster than opening the file and looking for the title.
    # It's not as pretty, but you can't have everything.
    $pageName =~ /(.*)\.html/;
    $1;

#     my($title);
#     open(FILE, "WebWiki/$pageName");
#     while (<FILE>) {
# 	if (/<title>(.*)<\/title>/) {
# 	    $title = $1;
# 	    break;
# 	}
#     }
#     close(FILE);
#     $title;
}

sub header {
    print <<EOF;
Content-Type: text/html


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
<head>
  <title>Recent Changes</title>
  <link rel="sytlesheet" type="text/css" href="emacs-wiki.css" />
</head>
<body>

<table width='100%'>
<tr>
  <td align='left'><h1><a href='WebWiki/WelcomePage.html'><img
    src='images/Keymaster.gif' border='0'></a>
    Recent Changes</h1></td>
  <td align='right'>
    Recent Changes
    <a href="wiki_search.html">Search</a>
  </td>
</tr>
</table>
<hr />
EOF
}

sub footer {
    print <<EOF;
</body>
</html>
EOF
}
