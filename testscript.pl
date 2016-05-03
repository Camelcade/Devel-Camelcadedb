#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use LWP::UserAgent;

my $scalar = 123;
my @array = (1, 2, 3, 5, 6);
my $ua = LWP::UserAgent->new();
my $regexp = qr/something/;
my %hash = (
    test    => 69,
    scref   => \$scalar,
    aref    => \@array,
    cref    => sub {print "blah!";},
    browser => LWP::UserAgent->new(),
    regex   => qr/somethingelse/,
);

use Testmod;
use Submod::Testmod;

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
