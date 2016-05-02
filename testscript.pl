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
    print 69 ."\n";
}

mysub();

