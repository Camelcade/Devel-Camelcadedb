use 5.008;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME               => 'Devel::Camelcadedb',
    AUTHOR             => q{Alexandr Evstigneev <hurricup@gmail.com>},
    VERSION_FROM       => 'lib/Devel/Camelcadedb.pm',
    ABSTRACT_FROM      => 'lib/Devel/Camelcadedb.pm',
    LICENSE            => 'MIT',
    PL_FILES           => { },
    MIN_PERL_VERSION   => 5.008,
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => 0,
    },
    BUILD_REQUIRES     => {
        'Test::More' => 0,
    },
    PREREQ_PM          => {
        Cwd                => 0,
        Encode             => 0,
        'Scalar::Util'     => 0,
        'Scalar::Util'     => 0,
        'B::Deparse'       => 0,
        'IO::Socket::INET' => 0,
        PadWalker          => 2.2,
        'JSON::XS'         => 3.02,
    },
    dist               => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean              => { FILES => 'Devel-Camelcadedb-*' },
    META_MERGE         => {
        'meta-spec' => {
            version => 2
        },
        resources   => {
            license    => [
                'http://www.opensource.org/licenses/mit-license.php',
            ],
            bugtracker => {
                web => 'https://github.com/hurricup/Perl5-IDEA-Debugger/issues'
            },
            repository => {
                type => 'git',
                url  => 'git@github.com:hurricup/Perl5-IDEA-Debugger.git',
                web  => 'https://github.com/hurricup/Perl5-IDEA-Debugger',
            },
        },
    },
);
