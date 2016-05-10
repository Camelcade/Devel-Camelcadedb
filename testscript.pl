#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use LWP::UserAgent;
use v5.10;
use Carp;

our $someglobal;
state $somestate;

format SOMEFORMAT =
This is format
.

my $ver = \$^V;
my $otherver;
{
    no strict 'refs';
    $otherver = *{'::^V'}{SCALAR};
}
my $glob = \*::;
my $deep_object = \\\\\\\LWP::UserAgent->new();
my $regexp = qr/something/;
my $scalar = 123;
my $ua = LWP::UserAgent->new();
my %hash = (
    test    => 69,
    scref   => \$scalar,
    cref    => sub {print "blah!";},
    browser => LWP::UserAgent->new(),
    regex   => qr/somethingelse/,
);
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
        format     => *SOMEFORMAT,
        version    => $^V,

    },
    1,
    5,
    6,
    'some',
    'test',

);

use Testmod;
use Submod::Testmod;

sub mysub
{
    print 42 ."\n";
    my $something = 123;
    my $otherone = 5;

    mysub2( 234 );

    foreach my $element (@array)
    {
        say $element;
    }

    $something++;
    $otherone += $something;

    $ua = LWP::UserAgent->new();
    $something = 'Hi there';
    $something .= $otherone;

    goto & mysub2;
}

sub mysub2
{

    print 69 ."\n";
}

mysub( 123 );
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
