#---------------------------------------------------------------------------------------------------
# This Perl script is used to extract different sub-grammars from a grammar like grammar.pl. Prolog
# comments of the form "%% ..." are used to define different sub-grammars. For example "%% m t"
# means that the following lines belong to the sub-grammars "m" and "t". The following command
# extracts the subgrammar "m" and writes the result into the file "grammar-m.pl":
#   perl extract_subgrammar.perl m > ../grammar-m.pl
#
# Author: Tobias Kuhn
#---------------------------------------------------------------------------------------------------

use strict;

my $s = $ARGV[0];
my $f = "";

while(<STDIN>) {
	if ( s/^%%(.*)$/$1/ ) {
		$f = $_;
	} elsif ($f eq "") {
		print;
	} elsif ($f =~ / $s/) {
		print;
	}
}
