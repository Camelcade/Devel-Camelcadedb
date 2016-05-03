#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

require LWP::UserAgent;

sub mysub
{
    print 42 ."\n";
    goto & mysub2;
}

sub mysub2
{
    my $ua = LWP::UserAgent->new();
    print 69 ."\n";
}

mysub();

eval {
    print "Eval is here\n";
};

eval q{
    print "String eval is here\n";
    };

print "Hi there\n";

BEGIN{
    print "THis is begin block!\n";
}
