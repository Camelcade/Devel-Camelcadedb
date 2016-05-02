use 5.008;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME             => 'Devel::Camelcadedb',
    AUTHOR           => q{Alexandr Evstigneev <hurricup@gmail.com>},
    VERSION_FROM     => 'lib/Devel/Camelcadedb.pm',
    ABSTRACT_FROM    => 'lib/Devel/Camelcadedb.pm',
    LICENSE          => 'perl_5',
    PL_FILES         => {},
    MIN_PERL_VERSION => 5.008,
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => 0,
    },
    BUILD_REQUIRES => {
        'Test::More' => 0,
    },
    PREREQ_PM => {
        #'ABC'              => 1.6,
        #'Foo::Bar::Module' => 5.0401,
    },
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'Devel-Camelcadedb-*' },
);
