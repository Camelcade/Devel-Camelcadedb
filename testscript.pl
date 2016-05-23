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

my @newlines = (
    "\n",
    "text\nab",
    'text\nab',
    "text\\n\nab",
);

my ($var_1, $var_2, $var_3, $var_4) =
    ("\n",
        "text\nab",
        'text\nab',
        "text\\n\nab",);

my $rus_words = '������!';
my $rus_utf_words = $rus_words;
Encode::from_to( $rus_utf_words, 'cp1251', 'utf8' );
utf8::decode( $rus_utf_words );

my $var = 123;
$var = 345;
$var = 567;

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
    newlines
    => \@newlines
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

    package Some {
        foreach(@array)
        {
            say;
        }
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

mysub(
    key1 => 'val1',
    key2 => 'val2',
    key3 => 'val3',
    key4 => 'val4',
    key5 => 'val5',
    key6 => 'val6',
);
somethingTestmod();

foreach my $element (1 .. 5)
{
    eval {
        print "Eval is here\n";
    };
}

eval <<'WOM';
package custompackage;
sub somevalsub()
 {
 print "custompackage sub\n";
 }

print "Sub created";
WOM

foreach my $element (1 .. 5)
{
    eval q{
    print "String eval is here\n";
    custompackage::somevalsub();
    };
}

foreach my $element (1 .. 5)
{
    #    custompackage::somevalsub();
}

print "Hi there\n";

BEGIN{
    print "THis is begin block!\n";
}
