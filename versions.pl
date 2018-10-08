#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use version;
use v5.10;

say version->parse('v100.200.300')->normal;
say version->parse('100.200.300')->normal;
say version->parse(100.200.300)->normal;
say version->parse('v1.2.3')->normal;
say version->parse('1.2.3')->normal;
say version->parse(1.2.3)->normal;
say version->parse('v1.2')->normal;
say version->parse('1.2')->normal;
say version->parse(1.2)->normal;
