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

done_testing();