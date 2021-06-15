#!perl -T
use v5.10;
use strict;
use warnings;
use Test::More;
use Data::Dumper;

subtest "Cyclic reference" => sub{
    $ENV{PERL5_DEBUG_AUTOSTART} = 0;
    $ENV{PERL5_DEBUG_ROLE} = 'server';
    $ENV{PERL5_DEBUG_HOST} = 'localhost';
    $ENV{PERL5_DEBUG_PORT} = 42;
    require Devel::Camelcadedb;

    my $reference = 'test';
    my $reference2 = \$reference;
    my $reference3 = \$reference2;
    $reference = \$reference3;
    DB::_get_reference_descriptor("testname", $reference);
    pass();
};

done_testing();