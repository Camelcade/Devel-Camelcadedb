#!perl -T
use 5.008;
use strict;
use warnings;
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'Devel::Camelcadedb' ) || print "Bail out!\n";
}

diag( "Testing Devel::Camelcadedb $Devel::Camelcadedb::VERSION, Perl $], $^X" );
