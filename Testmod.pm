sub somethingTestmod {
    print "here is something";
    my $ua = LWP::UserAgent->new();
}

print "bom";
my $ua = LWP::UserAgent->new();

BEGIN {

    print "Testmod begin";
}



1;
