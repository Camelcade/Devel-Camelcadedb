#!perl -T
use v5.10;
use strict;
use warnings;
use Test::More;
use Data::Dumper;
$Data::Dumper::Sortkeys = \1;
$Data::Dumper::Deepcopy = \1;

my $OVERWRITE_RESULTS = 0;

sub check_results_with_file {
    my $test_name = shift;
    my $result = shift;

    $result =~ s/(^\s+|\r|\s+$)//gsi;
    $result =~ s/^##teamcity/teamcity/gm;
    $result =~ s/(HASH|REF|SCALAR|ARRAY)\(0x[a-f0-9]+\)/$1(...)/gs;
    if ($^O eq 'MSWin32') {
        $result =~ s{\\}{/}gs;
    }
    my $result_file_path = "testData/results/$test_name.txt";
    if (!$OVERWRITE_RESULTS && -f $result_file_path) {
        open my $if, $result_file_path || fail("Error creating output file: $result_file_path, $!");
        my $expected = join '', <$if>;
        close $if;
        $expected =~ s/(^\s+|\s+$)//gsi;
        is($result, $expected, $test_name);
    }
    else {
        open my $of, ">$result_file_path" || fail("Error creating output file: $result_file_path, $!");
        print $of $result;
        close $of;
        fail($test_name);
        print STDERR "Output file is missing. Created a $result_file_path\n";
    }
}

sub setup_debugger {
    $ENV{PERL5_DEBUG_AUTOSTART} = 0;
    $ENV{PERL5_DEBUG_ROLE} = 'server';
    $ENV{PERL5_DEBUG_HOST} = 'localhost';
    $ENV{PERL5_DEBUG_PORT} = 42;
    require Devel::Camelcadedb;
}

subtest "Cyclic reference" => sub {
    setup_debugger();

    my $reference = 'test';
    my $reference2 = \$reference;
    my $reference3 = \$reference2;
    $reference = \$reference3;
    DB::_get_reference_descriptor("testname", $reference);
    pass();
};

subtest "Object Descriptor" => sub {
    setup_debugger();

    my $something = bless { foo => 42 }, 'Foo::Bar';
    my $scalar = 42;
    my @array = (42);
    my %hash = (key => 42);
    my $scalar_ref = \$scalar;
    my $array_ref = \@array;
    my $hash_ref = \%hash;

    use PadWalker qw/peek_my/;
    my $my_variables = peek_my(0);
    my $my_variables_descriptor = DB::_format_variables_hash($my_variables);

    my $result = "Object: \n" . Dumper($something) . "\n";

    my $descriptor = DB::_get_reference_descriptor("something", $something);
    $result .= "\nDescriptor: \n" . Dumper($descriptor) . "\n";

    $result .= "\nMy variables descriptor: \n" . Dumper($my_variables_descriptor) . "\n";

    ok($descriptor->{'expandable'}, "Object descriptor is expandable");
    my $subelements = DB::_compute_reference_subelements({
        offset => 0,
        limit  => 100,
        key    => $descriptor->{key}
    });
    $result .= "\nSubelements:\n" . Dumper($subelements);

    check_results_with_file("object_descriptor", $result);
};

subtest "Scalar rendering" => sub {
    setup_debugger();

    # A number that was also used as a string keeps IOK/NOK while gaining POK: it must still render
    # as a number, matching its runtime numeric semantics (see Camelcade/Perl5-IDEA#3198).
    my $stringified_number = 100;
    my $ignore = "$stringified_number";

    # A numeric-looking string mutated in numeric context stores the IV back and becomes a number.
    my $numified_string = "42";
    $numified_string += 0;

    my %values = (
        a_integer          => 42,
        b_negative_integer => -7,
        c_zero             => 0,
        d_float            => 3.5,
        e_negative_float   => -0.5,
        f_string           => "hello",
        g_numeric_string   => "42",           # POK only: a string that looks like a number, stays quoted
        h_float_string     => "3.5",          # POK only
        i_stringified_num  => $stringified_number, # IOK + POK: still a number
        j_numified_string  => $numified_string,    # POK + IOK after numeric mutation: shown as a number
        k_undef            => undef,
        l_utf8_string      => "\x{2603}",
    );

    my $hash_descriptor = DB::_get_reference_descriptor("values", \%values);
    my $subelements = DB::_compute_reference_subelements({
        offset => 0,
        limit  => 100,
        key    => $hash_descriptor->{key}
    });

    my $result = "Hash subelements (SCALAR ref branch):\n" . Dumper($subelements);

    # Raw scalar values go through the non-reference branch of _get_reference_descriptor.
    my @raw_cases = (
        [ integer        => 42 ],
        [ float          => 3.5 ],
        [ string         => "hello" ],
        [ numeric_string => "42" ],
        [ undef_value    => undef ],
    );
    $result .= "\nRaw scalars (non-reference branch):\n";
    $result .= Dumper([ map DB::_get_reference_descriptor(@$_), @raw_cases ]);

    check_results_with_file("scalar_rendering", $result);
};

done_testing();