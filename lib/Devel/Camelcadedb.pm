# http://perldoc.perl.org/DB.html
# http://perldoc.perl.org/perldebug.html
# http://perldoc.perl.org/perldebtut.html
# http://perldoc.perl.org/perldebguts.html
package DB;
use 5.008;
use strict;
use warnings;
use Data::Dumper;
use B::Deparse;
use Socket;

sub _passthrough_code(&;@);
sub _pass_through();

my $_debug_server_host;     # remote debugger host
my $_debug_server_port;     # remote debugger port

my $_local_debug_host;      # local debug host
my $_local_debug_port;      # local debug port

my $_debug_net_role;        # server or client, we'll use ENV for this
my $_debug_socket;
my $_debug_packed_address;

my $_debugger_inited;
my $_deparser;

BEGIN{
    use constant {
        FLAG_SUB_ENTER_EXIT         => 0x01,
        FLAG_LINE_BY_LINE           => 0x02,
        FLAG_OPTIMIZATIONS_OFF      => 0x04,
        FLAG_MORE_DATA              => 0x08,
        FLAG_SOURCE_LINES           => 0x10,
        FLAG_SINGLE_STEP_ON         => 0x20,
        FLAG_USE_SUB_ADDRESS        => 0x40,
        FLAG_REPORT_GOTO            => 0x80,
        FLAG_EVAL_FILENAMES         => 0x100,
        FLAG_ANON_FILENAMES         => 0x200,
        FLAG_STORE_SOURCES          => 0x400,
        FLAG_KEEP_SUBLESS_EVALS     => 0x800,
        FLAG_KEEP_UNCOMPILED_SOURCE => 0x1000,
        FLAGS_DESCRIPTIVE_NAMES => [
            q{Debug subroutine enter/exit.},
            q{Line-by-line debugging. Causes DB::DB() subroutine to be called for each statement executed. Also causes saving source code lines (like 0x400).}
            ,
            q{Switch off optimizations.},
            q{Preserve more data for future interactive inspections.},
            q{Keep info about source lines on which a subroutine is defined.},
            q{Start with single-step on.},
            q{Use subroutine address instead of name when reporting.},
            q{Report goto &subroutine as well.},
            q{Provide informative "file" names for evals based on the place they were compiled.},
            q{Provide informative names to anonymous subroutines based on the place they were compiled.},
            q{Save source code lines into @{"_<$filename"}.},
            q{When saving source, include evals that generate no subroutines.},
            q{When saving source, include source that did not compile.},
        ],
        STEP_CONTINUE           => 0,
        STEP_INTO               => 1,
        STEP_OVER               => 2,
    };

    my $internals_keys = [ qw/
        FLAG_SUB_ENTER_EXIT
        FLAG_LINE_BY_LINE
        FLAG_OPTIMIZATIONS_OFF
        FLAG_MORE_DATA
        FLAG_SOURCE_LINES
        FLAG_SINGLE_STEP_ON
        FLAG_USE_SUB_ADDRESS
        FLAG_REPORT_GOTO
        FLAG_EVAL_FILENAMES
        FLAG_ANON_FILENAMES
        FLAG_STORE_SOURCES
        FLAG_KEEP_SUBLESS_EVALS
        FLAG_KEEP_UNCOMPILED_SOURCE
        FLAGS_DESCRIPTIVE_NAMES
        _report
        BEGIN
        DB
        sub
        lsub
        postponed
        goto
        args
        Dumper
        dbline
        trace
        single
        signal
        / ];
    my $internals = { };
    @$internals{@$internals_keys} = @$internals_keys;

    # Each array @{"_<$filename"} holds the lines of $filename for a file compiled by Perl. The same is also true for evaled
    # strings that contain subroutines, or which are currently being executed. The $filename for evaled strings looks like
    # (eval 34) .
    # Values in this array are magical in numeric context: they compare equal to zero only if the line is not breakable.
    #
    # Each hash %{"_<$filename"} contains breakpoints and actions keyed by line number. Individual entries (as opposed to
    # the whole hash) are settable. Perl only cares about Boolean true here, although the values used by perl5db.pl have the
    # form "$break_condition\0$action" .
    #
    # The same holds for evaluated strings that contain subroutines, or which are currently being executed. The $filename
    # for evaled strings looks like (eval 34) .
    #
    # Each scalar ${"_<$filename"} contains "_<$filename" . This is also the case for evaluated strings that contain
    # subroutines, or which are currently being executed. The $filename for evaled strings looks like (eval 34) .
    #
    # DB::dump_trace(skip[,count]) skips the specified number of frames and returns a list containing information about the
    # calling frames (all of them, if count is missing). Each entry is reference to a hash with keys context (either ., $ ,
    # or @ ), sub (subroutine name, or info about eval), args (undef or a reference to an array), file , and line .
    #

    # these are hardcoded in perl source (some are magical)

    # When execution of the program reaches a subroutine call, a call to &DB::sub (args) is made instead, with $DB::sub
    # holding the name of the called subroutine. (This doesn't happen if the subroutine was compiled in the DB package.)
    our $sub = '';        # Name of current executing subroutine.

    # A hash %DB::sub is maintained, whose keys are subroutine names and whose values have the form
    # filename:startline-endline . filename has the form (eval 34) for subroutines defined inside evals.
    #
    # The keys of this hash are the names of all the known subroutines. Each value is an encoded string that has the
    # sprintf(3) format ("%s:%d-%d", filename, fromline, toline) .
    our %sub = ();        # "filename:fromline-toline" for every known sub

    # If you set $DB::single to 2, it's equivalent to having just typed the step over command, whereas a value of 1
    # means the step into command.
    our $single = 0;      # single-step flag (set it to 1 to enable stops in BEGIN/use) 1 -into, 2 - over

    # Signal flag. Will be set to a true value if a signal was caught. Clients may check for this flag to abort
    # time-consuming operations.
    our $signal = 0;

    # The $DB::trace variable should be set to 1 to simulate having typed the t command.
    # This flag is set to true if the API is tracing through subroutine calls.
    our $trace = 0;       # are we tracing through subroutine calls?

    # For example, whenever you call Perl's built-in caller function from the package DB , the arguments that the
    # corresponding stack frame was called with are copied to the @DB::args array. These mechanisms are enabled by calling
    # Perl with the -d switch. Specifically, the following additional features are enabled (cf. $^P in perlvar):
    our @args = ();       # arguments of current subroutine or @ARGV array

    our @ret = ();        # return value of last sub executed in list context
    our $ret = '';        # return value of last sub executed in scalar context

    #
    # # @DB::dbline is an alias for @{"::_<current_file"} , which holds the lines of the currently-selected file (compiled by
    # Perl), either explicitly chosen with the debugger's f command, or implicitly by flow of execution.
    #
    our @dbline = ();     # list of lines in currently loaded file

    # %DB::dbline is an alias for %{"::_<current_file"} , which contains breakpoints and actions keyed by line number in
    # the currently-selected file, either explicitly chosen with the debugger's f command, or implicitly by flow of execution.
    # As previously noted, individual entries (as opposed to the whole hash) are settable. Perl only cares about Boolean
    # true here, although the values used by perl5db.pl have the form "$break_condition\0$action" .
    #
    # Actions in current file (keys are line numbers). The values are strings that have the sprintf(3) format
    # ("%s\000%s", breakcondition, actioncode) .
    our %dbline = ();     # actions in current file (keyed by line number)

    $_debugger_inited = 0;
    $_local_debug_port = 12345;
    $_debug_net_role = 'server';
    $_deparser = B::Deparse->new();


    # http://perldoc.perl.org/perlipc.html#Sockets%3a-Client%2fServer-Communication
    if ($_debug_net_role eq 'server')
    {
        my $_server_socket;
        socket( $_server_socket, PF_INET, SOCK_STREAM, getprotobyname 'tcp' ) || die "socket $!";
        setsockopt( $_server_socket, SOL_SOCKET, SO_REUSEADDR, pack( 'l', 1 ) ) || die "socketopt $!";
        bind( $_server_socket,
            sockaddr_in( $_local_debug_port, $_local_debug_host ? inet_aton( $_local_debug_host ) : INADDR_ANY )
        ) || die "bind $!";
        listen( $_server_socket, SOMAXCONN ) || die "listen $!";
        $_debug_packed_address = accept( $_debug_socket, $_server_socket );
    }
    else
    {
        socket( $_debug_socket, PF_INET, SOCK_STREAM, getprotobyname 'tcp' ) || die "socket: $!";
        connect( $_debug_socket,
            sockaddr_in( $_debug_server_port, 'tcp', inet_aton( $_debug_server_host ) )
        ) || die "connect:  $!";
    }

    $^P |= FLAG_REPORT_GOTO;

    sub _report($;@)
    {
        my ($message, @sprintf_args) = @_;
        printf STDERR "$message\n", @sprintf_args;
    }

    #    _report "* Debugger settings:";
    #    for (my $i = 0; $i < @{FLAGS_DESCRIPTIVE_NAMES()}; $i++)
    #    {
    #        _report " * ".FLAGS_DESCRIPTIVE_NAMES->[$i] if ($^P & (1 << $i));
    #    }
}

sub _init_debugger()
{
    _passthrough_code {
            # for now it's just skipping debugger loading
            $_debugger_inited = 1;
            print STDERR "Debugger inited \n";
        };
}


# When the execution of your program reaches a point that can hold a breakpoint, the DB::DB() subroutine is called if
# any of the variables $DB::trace , $DB::single , or $DB::signal is true. These variables are not localizable. This
# feature is disabled when executing inside DB::DB() , including functions called from it unless $^D & (1<<30) is true.
my $_db_passthrough = 0;
sub DB
{
    return if _pass_through();

    $_db_passthrough = 1;
    my @saved = ($@, $!, $,, $/, $\, $^W);

    my ($package, $filename, $line_number) = caller;
    _report "* DB::DB called from %s:: %s line %s with %s, %s-%s-%s",
        $package // 'undef',
        $filename // 'undef',
        $line_number // 'undef',
        (join ',', @_) // '',
        $DB::trace // 'undef',
        $DB::signal // 'undef',
        $DB::single // 'undef',
    ;

    my $srcline;
    {
        no strict 'refs';
        $srcline = ${"::_<$filename"}[$line_number];
    }
    print STDERR $srcline;

    my $command = <$_debug_socket>;
    die 'Debugging socket disconnected' if !defined $command;
    $command =~ s/[\r\n]+$//;
    print STDERR "Got command: '$command'\n";

    if ($command eq 'Q')
    {
        print STDERR "Exiting";
        exit;
    }
    elsif ($command eq 'GO')
    {
        $DB::single = STEP_CONTINUE;
    }
    elsif ($command eq 'OV') # over
    {
        $DB::single = STEP_OVER;
    }
    else
    {
        $DB::single = STEP_INTO;
    }

    $_db_passthrough = 0;
    ($@, $!, $,, $/, $\, $^W) = @saved;
    return;
}


my $_sub_passthrough = 0;
sub sub
{
    my $old_db_single = $DB::single;

    if (!_pass_through())
    {
        my @saved = ($@, $!, $,, $/, $\, $^W);
        $_sub_passthrough = 1;

        my ($package, $file, $line) = caller;
        _report "* DB::sub called %s%s from %s:: %s line %s %s-%s-%s",
            $DB::sub,
                scalar @_ ? ' with '.(join ',', @_) : '',
            $package // 'undef',
            $file // 'undef',
            $line // 'undef',
            $DB::trace // 'undef',
            $DB::signal // 'undef',
            $DB::single // 'undef',
        ;

        #        if( ref $DB::sub )
        #        {
        #            print STDERR _get_deparsed_target(); # these are BEGIN blocks
        #        }
        if ($DB::single == STEP_OVER)
        {
            print STDERR "Disabling step in 1\n";
            $DB::single = STEP_CONTINUE;
        }

        $_sub_passthrough = 0;
        ($@, $!, $,, $/, $\, $^W) = @saved;
    }

    if (wantarray)
    {
        no strict 'refs';
        @DB::ret = &$DB::sub;

        if ($old_db_single != $DB::single)
        {
            print STDERR "Enabling setp in $DB::single => $old_db_single\n";
            $DB::single = $old_db_single;
        }

        return @DB::ret;
    }
    elsif (defined wantarray)
    {
        no strict 'refs';
        $DB::ret = &$DB::sub;

        if ($old_db_single != $DB::single)
        {
            print STDERR "Enabling setp in $DB::single => $old_db_single\n";
            $DB::single = $old_db_single;
        }

        return $DB::ret;
    }
    else
    {
        no strict 'refs';
        &$DB::sub;
        $DB::ret = undef;

        if ($old_db_single != $DB::single)
        {
            print STDERR "Enabling setp in $DB::single => $old_db_single\n";
            $DB::single = $old_db_single;
        }

        return;
    }
}

# If the call is to an lvalue subroutine, and &DB::lsub is defined &DB::lsub (args) is called instead, otherwise
# falling back to &DB::sub (args).
my $_lsub_passthrough = 0;
sub lsub: lvalue
{
    if (!_pass_through())
    {
        my @saved = ($@, $!, $,, $/, $\, $^W);
        $_lsub_passthrough = 1;

        my ($package, $file, $line) = caller;
        #        _report "* DB::lsub called %s%s from %s:: %s line %s %s-%s-%s",
        #            $DB::sub,
        #                scalar @_ ? ' with '.(join ',', @_) : '',
        #            $package // 'undef',
        #            $file // 'undef',
        #            $line // 'undef',
        #            $DB::trace // 'undef',
        #            $DB::signal // 'undef',
        #            $DB::single // 'undef',
        #        ;

        #        if( ref $DB::sub )
        #        {
        #            print STDERR _get_deparsed_target();    # these are BEGIN blocks
        #        }

        $_lsub_passthrough = 0;
        ($@, $!, $,, $/, $\, $^W) = @saved;
    }
    &$DB::sub;
}

# After each required file is compiled, but before it is executed, DB::postponed(*{"_<$filename"}) is called if the
# subroutine DB::postponed exists. Here, the $filename is the expanded name of the required file, as found in the values
# of %INC.
#
# After each subroutine subname is compiled, the existence of $DB::postponed{subname} is checked. If this key exists,
# DB::postponed(subname) is called if the DB::postponed subroutine also exists.
my $_postponed_passthrough = 0;
sub postponed
{
    my @saved = ($@, $!, $,, $/, $\, $^W);

    my $old_postponed_passthrough = $_postponed_passthrough;
    $_postponed_passthrough = 1;

    if ($_debugger_inited)
    {
        my ($package, $file, $line) = caller;
        #        _report "* DB::postponed called%s from %s:: %s line %s %s-%s-%s",
        #                scalar @_ ? ' with '.(join ',', @_) : '',
        #            $package // 'undef',
        #            $file // 'undef',
        #            $line // 'undef',
        #            $DB::trace // 'undef',
        #            $DB::signal // 'undef',
        #            $DB::single // 'undef',
        #        ;
    }
    else
    {
        _init_debugger();
    }
    $_postponed_passthrough = $old_postponed_passthrough;

    ($@, $!, $,, $/, $\, $^W) = @saved;
}
# When execution of the program uses goto to enter a non-XS subroutine and the 0x80 bit is set in $^P , a call to
# &DB::goto is made, with $DB::sub holding the name of the subroutine being entered.
my $_goto_passthrough = 0;
sub goto
{
    return if _pass_through();

    my @saved = ($@, $!, $,, $/, $\, $^W);
    my ($package, $file, $line) = caller;

    if (!$package || $package ne 'DB')
    {
        $_goto_passthrough = 1;
        _report "* DB::goto called%s from %s:: %s line %s %s-%s-%s",
                scalar @_ ? ' with '.(join ',', @_) : '',
            $package // 'undef',
            $file // 'undef',
            $line // 'undef',
            $DB::trace // 'undef',
            $DB::signal // 'undef',
            $DB::single // 'undef',
        ;
        $_goto_passthrough = 0;
    }
    ($@, $!, $,, $/, $\, $^W) = @saved;
}

sub _pass_through()
{
    return $_db_passthrough || $_sub_passthrough || $_lsub_passthrough || $_goto_passthrough || $_postponed_passthrough;
}

sub _get_deparsed_target()
{
    return _passthrough_code {
            eval {$_deparser->coderef2text( $DB::sub )};
        };
}

sub _passthrough_code(&;@)
{
    my $coderef = shift;
    my $old_db_passthrgouth = $_db_passthrough;
    $_db_passthrough = 1;
    my $res = $coderef->( @_ );
    $_db_passthrough = $old_db_passthrgouth;
    return $res;
}

1; # End of Devel::Camelcadedb
