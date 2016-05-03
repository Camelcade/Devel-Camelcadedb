# http://perldoc.perl.org/DB.html
# http://perldoc.perl.org/perldebug.html
# http://perldoc.perl.org/perldebtut.html
# http://perldoc.perl.org/perldebguts.html
package DB;
use 5.008;
use strict;
use warnings;
use Data::Dumper;
use PadWalker qw/peek_sub peek_my/;
use Scalar::Util qw/reftype/;
use B::Deparse;
use Cwd;
use Socket;
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
    FLAGS_DESCRIPTIVE_NAMES     => [
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
        q{Save source code lines into @{"::_<$filename"}.},
        q{When saving source, include evals that generate no subroutines.},
        q{When saving source, include source that did not compile.},
    ],
    STEP_CONTINUE               => 0,
    STEP_INTO                   => 1,
    STEP_OVER                   => 2,
};

# Each array @{"::_<$filename"} holds the lines of $filename for a file compiled by Perl. The same is also true for evaled
# strings that contain subroutines, or which are currently being executed. The $filename for evaled strings looks like
# (eval 34) .
# Values in this array are magical in numeric context: they compare equal to zero only if the line is not breakable.
#
# # @DB::dbline is an alias for @{"::_<current_file"} , which holds the lines of the currently-selected file (compiled by
# Perl), either explicitly chosen with the debugger's f command, or implicitly by flow of execution.
#
our @dbline = ();     # list of lines in currently loaded file

# Each hash %{"::_<$filename"} contains breakpoints and actions keyed by line number. Individual entries (as opposed to
# the whole hash) are settable. Perl only cares about Boolean true here, although the values used by perl5db.pl have the
# form "$break_condition\0$action" .
#
# The same holds for evaluated strings that contain subroutines, or which are currently being executed. The $filename
# for evaled strings looks like (eval 34) .
#
# %DB::dbline is an alias for %{"::_<current_file"} , which contains breakpoints and actions keyed by line number in
# the currently-selected file, either explicitly chosen with the debugger's f command, or implicitly by flow of execution.
# As previously noted, individual entries (as opposed to the whole hash) are settable. Perl only cares about Boolean
# true here, although the values used by perl5db.pl have the form "$break_condition\0$action" .
#
# Actions in current file (keys are line numbers). The values are strings that have the sprintf(3) format
# ("%s\000%s", breakcondition, actioncode) .
our %dbline = ();     # actions in current file (keyed by line number)

# Each scalar ${"::_<$filename"} contains "::_<$filename" . This is also the case for evaluated strings that contain
# subroutines, or which are currently being executed. The $filename for evaled strings looks like (eval 34) .
#
our $dbline;

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

my %_real_filenames = ();   # map of debugger path => real path detected on loading

my $_debug_server_host;     # remote debugger host
my $_debug_server_port;     # remote debugger port

my $_local_debug_host;      # local debug host
my $_local_debug_port;      # local debug port

my $_debug_net_role;        # server or client, we'll use ENV for this
my $_debug_socket;
my $_debug_packed_address;

my $current_line;
my $current_package;
my $current_file;

my $_stack_frames = [ ];     # stack frames

my $_debugger_inited;
my $_deparser;

sub _report($;@)
{
    my ($message, @sprintf_args) = @_;
    printf STDERR "$message\n", @sprintf_args;
}

sub _render_variables
{
    my ($vars_hash) = @_;
    my $result = '';

    foreach my $variable (keys %$vars_hash)
    {
        my $value = $vars_hash->{$variable};

        my $reftype = reftype $value;
        my $ref = ref $value;
        my $appendix = '';

        if ($reftype eq 'SCALAR')
        {
            $value = $$value // '_UNDEF_';
        }
        elsif ($reftype eq 'ARRAY')
        {
            $appendix = sprintf '[%s]', scalar @$value;
            my @elements = @$value;
            if (@elements > 1000)
            {
                splice @elements, 3; # should be a 1000 here and method to get the rest
                push @elements, '...'
            }
            $value = sprintf "(%s)", join ',', map { $_ // '__UNDEF__'} @elements;
        }
        elsif ($reftype eq 'HASH')
        {
            my @elements = sort keys %$value; # sorting is necessary to iterate and get data frame
            $appendix = sprintf '{%s}', scalar @elements;

            if (@elements > 1000)
            {
                splice @elements, 3; # should be a 1000 here and method to get the rest
                push @elements, '...'
            }
            $value = sprintf "(\n%s\n)", join ",\n", map { "\t\t".$_." => ".$value->{$_}} @elements;
        }
        elsif (ref $value eq 'REF')
        {
            $value = $$value;
        }

        $result .= "\t$ref $variable$appendix = $value\n";
    }

    return $result;
}

sub _event_handler
{
    while()
    {
        my $command = <$_debug_socket>;
        die 'Debugging socket disconnected' if !defined $command;
        $command =~ s/[\r\n]+$//;
        print STDERR "Got command: '$command'\n";

        if ($command eq 'q')
        {
            print STDERR "Exiting";
            exit;
        }
        elsif ($command eq 'l') # list
        {
            for (my $i = 0; $i < @DB::dbline; $i++)
            {
                my $src = $DB::dbline[$i] // '';
                chomp $src;
                printf STDERR "%s: %s (%s)\n", $i, $src, $DB::dbline[$i] == 0 ? 'unbreakable' : 'breakable';
            }
        }
        elsif ($command eq 's') # dump %sub
        {
            print STDERR Dumper( \%DB::sub );
        }
        elsif ($command eq 'f') # dump keys of %DB::dbline
        {
            print STDERR Dumper( \%_real_filenames );
        }
        elsif ($command eq 'g')
        {
            foreach my $frame (@{$_stack_frames})
            {
                $frame->{_single} = STEP_CONTINUE;
            }
            $DB::single = STEP_CONTINUE;
            return;
        }
        elsif ($command eq 'b') # show breakpoints
        {
            no strict 'refs';
            print Dumper( \%DB::dbline );
        }
        elsif ($command =~ /^b (\d+)$/)
        {
            my $line = $1;
            if ($DB::dbline[$line] == 0)
            {
                print STDERR "Line $line is unbreakable, try another one\n";
            }
            else
            {
                if ($DB::dbline{$line})
                {
                    $DB::dbline{$line} = 0;
                    print STDERR "Removed breakpoint from line $line\n";
                }
                else
                {
                    $DB::dbline{$line} = 1;
                    print STDERR "Added breakpoint to line $line\n";
                }
            }
        }
        elsif ($command eq 'v') # show variables
        {
            _report " * Lexical variables:\n%s", _render_variables( peek_my( 2 ) );
        }
        elsif ($command eq 'o') # over,
        {
            # this approach won't work, because it can be use/no statement or, BEGIN statement
            # basically, anything generated by parser
            #            my $frame = _get_current_frame();
            #            if (ref $frame->{subname}) # we are in use compiled block, omit everything till the end of frame, we need to handle use statement
            #            {
            #                _report("We are in code block: %s", $frame->{subname});
            #                $DB::single = STEP_CONTINUE;
            #            }
            #            else
            #            {
            #                _report("We are in normal sub: %s", $frame->{subname});
            #                $DB::single = STEP_OVER;
            #            }
            $DB::single = STEP_OVER;
            return;
        }
        elsif ($command eq 'u') # step out
        {
            #            my $frame = _get_current_frame;
            #
            #            if (ref $frame->{subname}) # we are in use compiled block, omit everything till the end of frame
            #            {
            #                $frame->{_single} = STEP_CONTINUE; # should go out of
            #            }
            $DB::single = STEP_CONTINUE;
            return;
        }
        elsif ($command eq 't') # stack trace
        {
            _report( "* Stack trace" );

            foreach my $frame (@$_stack_frames)
            {
                _report( "  * %s(%s) from %s, line %s",
                    $frame->{subname},
                    join( ', ', @{$frame->{args}} ),
                    $frame->{from_file},
                    $frame->{from_line}
                );

                #                my $coderef = $frame->{subname};
                #
                #                if( !ref $coderef)
                #                {
                #                    no strict 'refs';
                #                    $coderef = \&{$frame->{subname}};
                #                }
                #
                #                _report( 'Variables: %s', Dumper(peek_sub($coderef)));

                _report( "    * Lexical variables:\n%s", _render_variables( $frame->{_lexical_vars} ) );
            }
        }
        else
        {
            $DB::single = STEP_INTO;
            return;
        }
    }
}

sub _set_dbline
{
    no strict 'refs';
    *DB::dbline = *{"::_<$current_file"};
}

# When the execution of your program reaches a point that can hold a breakpoint, the DB::DB() subroutine is called if
# any of the variables $DB::trace , $DB::single , or $DB::signal is true. These variables are not localizable. This
# feature is disabled when executing inside DB::DB() , including functions called from it unless $^D & (1<<30) is true.
sub step_handler
{
    #    if( $DB::single == STEP_OVER ) fixme this might work with slight tuning
    #    {
    #        $DB::single = STEP_CONTINUE;
    #        return;
    #    }

    my $old_db_single = $DB::single;
    $DB::single = STEP_CONTINUE;
    my @saved = ($@, $!, $,, $/, $\, $^W);

    ($current_package, $current_file, $current_line) = caller;
    _set_dbline();

    _report "* Step at %s:: %s line %s with %s, %s-%s-%s, depth %s",
        $current_package // 'undef',
        $current_file // 'undef',
        $current_line // 'undef',
        (join ',', @_) // '',
        $DB::trace // 'undef',
        $DB::signal // 'undef',
        $old_db_single // 'undef',
        scalar @$_stack_frames,
    ;

    print STDERR $DB::dbline[$current_line];

    _event_handler( );

    ($@, $!, $,, $/, $\, $^W) = @saved;
    return;
}

sub _get_current_frame
{
    return $_stack_frames->[0];
}

my $_frame_passthrough = 0;
sub _enter_frame
{
    my @saved = ($@, $!, $,, $/, $\, $^W);
    my ($args_ref) = @_;
    _set_dbline();

    my $_current_stack_frame = {
        subname       => $DB::sub,
        args          => $args_ref,
        from_file     => $current_file,
        from_line     => $current_line,
        _single       => $DB::single,
        _lexical_vars => peek_my( 1 ),
        _dbline       => *DB::dbline
    };
    unshift @$_stack_frames, $_current_stack_frame;
    $DB::single = STEP_CONTINUE;

    if (!$_frame_passthrough)
    {
        $_frame_passthrough = 1;
        _report "* Entering frame %s%s from %s:: %s line %s %s-%s-%s, depth: %s",
            $DB::sub,
                scalar @_ ? ' with '.(join ',', @_) : '',
            $current_package // 'undef',
            $current_file // 'undef',
            $current_line // 'undef',
            $DB::trace // 'undef',
            $DB::signal // 'undef',
            $_current_stack_frame->{_single} // 'undef',
            scalar @$_stack_frames,
        ;
        $_frame_passthrough = 0;
    }

    if ($_current_stack_frame->{_single} == STEP_OVER)
    {
        print STDERR "Disabling step in in subs\n";
        $DB::single = STEP_CONTINUE;
    }
    else
    {
        $DB::single = $_current_stack_frame->{_single};
    }
    ($@, $!, $,, $/, $\, $^W) = @saved;
    return $_current_stack_frame;
}

sub _exit_frame
{
    my @saved = ($@, $!, $,, $/, $\, $^W);
    _report " * Leaving frame, from: %s", scalar @$_stack_frames;
    my $frame = shift @$_stack_frames;
    $DB::single = $frame->{_single};
    *DB::dbline = $frame->{_dbline};
    ($@, $!, $,, $/, $\, $^W) = @saved;
}

# this pass-through flag handles quotation overload loop
sub sub_handler
{
    ($current_package, $current_file, $current_line) = caller;
    _enter_frame( [ @_ ] );

    if (wantarray)
    {
        no strict 'refs';
        @DB::ret = &$DB::sub;
        _exit_frame();
        return @DB::ret;
    }
    elsif (defined wantarray)
    {
        no strict 'refs';
        $DB::ret = &$DB::sub;
        _exit_frame();
        return $DB::ret;
    }
    else
    {
        no strict 'refs';
        &$DB::sub;
        $DB::ret = undef;
        _exit_frame();
        return;
    }
}

# If the call is to an lvalue subroutine, and &DB::lsub is defined &DB::lsub (args) is called instead, otherwise
# falling back to &DB::sub (args).
sub lsub_handler: lvalue
{
    ($current_package, $current_file, $current_line) = caller;
    _enter_frame( [ @_ ] );

    {
        no strict 'refs';
        $DB::ret = &$DB::sub;
        _exit_frame();
        return $DB::ret;
    }
}

# After each required file is compiled, but before it is executed, DB::postponed(*{"::_<$filename"}) is called if the
# subroutine DB::postponed exists. Here, the $filename is the expanded name of the required file, as found in the values
# of %INC.
#
# After each subroutine subname is compiled, the existence of $DB::postponed{subname} is checked. If this key exists,
# DB::postponed(subname) is called if the DB::postponed subroutine also exists.
sub load_handler
{
    my $old_db_single = $DB::single;
    $DB::single = STEP_CONTINUE;
    my @saved = ($@, $!, $,, $/, $\, $^W);

    ($current_package, $current_file, $current_line) = caller;
    _set_dbline();

    my $new_filename = $_[0];
    if ($new_filename =~ /_<(.+)$/)
    {
        my $path = $1;
        my $real_path = $path;
        if ($path !~ m{^(/|\w\:)})
        {
            # relative
            my $current_dir = getcwd;
            $real_path = "$current_dir/$path";
            print STDERR "$new_filename real path is $real_path\n";
        }
        else
        {
            print STDERR "$new_filename is absolute path\n";
        }
        $_real_filenames{$path} = $real_path;
    }
    else
    {
        die "Incorrect file: $new_filename";
    }

    _report "* DB::postponed called%s from %s:: %s line %s %s-%s-%s",
            scalar @_ ? ' with '.(join ',', @_) : '',
        $current_package // 'undef',
        $current_file // 'undef',
        $current_line // 'undef',
        $DB::trace // 'undef',
        $DB::signal // 'undef',
        $old_db_single // 'undef',
    ;

    ($@, $!, $,, $/, $\, $^W) = @saved;
    $DB::single = $old_db_single;
}
# When execution of the program uses goto to enter a non-XS subroutine and the 0x80 bit is set in $^P , a call to
# &DB::goto is made, with $DB::sub holding the name of the subroutine being entered.
sub goto_handler
{
    my $old_db_single = $DB::single;
    $DB::single = STEP_CONTINUE;

    my @saved = ($@, $!, $,, $/, $\, $^W);
    ($current_package, $current_file, $current_line) = caller;
    _set_dbline();

    if (!$current_package || $current_package ne 'DB')
    {
        _report "* DB::goto called%s from %s:: %s line %s %s-%s-%s",
                scalar @_ ? ' with '.(join ',', @_) : '',
            $current_package // 'undef',
            $current_file // 'undef',
            $current_line // 'undef',
            $DB::trace // 'undef',
            $DB::signal // 'undef',
            $old_db_single // 'undef',
        ;
    }
    ($@, $!, $,, $/, $\, $^W) = @saved;
    $DB::single = $old_db_single;
}


$_local_debug_host = 'localhost';
$_local_debug_port = 12345;
$_debug_net_role = 'server';
$_deparser = B::Deparse->new();

$^P |= FLAG_REPORT_GOTO;

# http://perldoc.perl.org/perlipc.html#Sockets%3a-Client%2fServer-Communication
if ($_debug_net_role eq 'server')
{
    _report( "Listening for the debugger at %s:%s...", $_local_debug_host, $_local_debug_port );
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
    _report( "Connecting to the debugger at %s:%s...", $_debug_server_host, $_debug_server_port );
    socket( $_debug_socket, PF_INET, SOCK_STREAM, getprotobyname 'tcp' ) || die "socket: $!";
    connect( $_debug_socket,
        sockaddr_in( $_debug_server_port, 'tcp', inet_aton( $_debug_server_host ) )
    ) || die "connect:  $!";
}

($current_package, $current_file, $current_line) = caller;
_set_dbline();
push @$_stack_frames, {
        subname       => '',
        args          => [ @ARGV ],
        from_file     => $current_file,
        from_line     => $current_line,
        _single       => STEP_INTO,
        _dbline       => *DB::dbline,
        _lexical_vars => { }
    };

getcwd; # this really loads something

*DB::DB = \&step_handler;
*DB::sub = \&sub_handler;
*DB::lsub = \&lsub_handler;
*DB::postponed = \&load_handler;
*DB::goto = \&goto_handler;
$DB::single = STEP_INTO;

1; # End of Devel::Camelcadedb
