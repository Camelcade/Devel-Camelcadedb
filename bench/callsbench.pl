#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use Time::HiRes;
use v5.10;

# this is a benchmarking script for checking debugger invocations overhead

# debug 25.96 sec, run 0.104 sec - 19/june/2016, 11:00 MSK

my $starttime = Time::HiRes::time();

sub step1 {}
sub step2 {step1}
sub step3 {step2}
sub step4 {step3}
sub step5 {step4}
sub step6 {step5}
sub step7 {step6}
sub step8 {step7}
sub step9 {step8}
sub step10 {step9}

for (0 .. 200_000)
{
    step10;
}

my $endtime = Time::HiRes::time();

say sprintf 'Finished in %s msec', ($endtime - $starttime);
