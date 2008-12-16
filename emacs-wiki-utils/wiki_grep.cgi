#! /usr/bin/env perl

use CGI;

$query = new CGI;
$searchText = $query->param('search');

header();
open(FILE, "wiki_search.html") || die "help! $!\n";
while (<FILE>) {
    print;
    print_search_results($searchText) if /<!-- RESULTS -->/;
}
print "<p>Done!</p>";
close(FILE);
exit(0);

sub print_search_results {
    my($searchText) = @_;
    my($fileName);

    print "<hr />\n";
    foreach $fileName (`grep -l -i "$searchText" WebWiki/*.html`) {
	$fileName = fileName($fileName);
	my($pageName) = pageName($fileName);
	print "<a href=\"WebWiki/$fileName\">$pageName</a><br />\n";
    }
}


# Given a full path with possible newline, return the file name.
sub fileName {
    my($fileName) = @_;
    chomp($fileName);		# Remove newline from full name
    $fileName = `basename $fileName`; # Remove newline; basename adds one
    chomp($fileName);
    $fileName;
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


EOF
}
