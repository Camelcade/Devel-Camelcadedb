#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use LWP::UserAgent;

my $scalar = 123;
my $deep_object = \\\\\\\LWP::UserAgent->new();
my @array = (
    {
        string     => 'is',
        string_ref => \'is',
        number     => 42,
        number_ref => \42,
        sub        => sub {print 'hi'},
        array_ref  => [ 8, 9, 10, 11 ],
        hash_ref   => { peek => 'me' },
        object     => LWP::UserAgent->new(),
        glob       => *::,
        regex      => qr/some/,
        handle     => *STDOUT,
    },
    1,
    5,
    6,
);
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
somethingTestmod();

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
