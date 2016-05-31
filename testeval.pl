#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use v5.10;

sub third
{
    say '+Third';
    die 'Here';
    say '-Third';
}

sub second
{
    say '+Second';
    eval {third};
    say '-Second';
}

sub first2
{
    say '+first2';
    eval {second};
    say '-first2';
}

sub first
{
    say '+First';
    first2;
    say '-First';
}

sub zero
{
    say '+Zero';
    first;
    say '-Zero';
}

zero;