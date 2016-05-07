# http://perldoc.perl.org/DB.html
# http://perldoc.perl.org/perldebug.html
# http://perldoc.perl.org/perldebtut.html
# http://perldoc.perl.org/perldebguts.html
package DB;
use 5.008;
use strict;
use warnings;
use IO::Socket::INET;
use PadWalker qw/peek_my/;
#use constant {
#    FLAG_SUB_ENTER_EXIT         => 0x01,
#    FLAG_LINE_BY_LINE           => 0x02,
#    FLAG_OPTIMIZATIONS_OFF      => 0x04,
#    FLAG_MORE_DATA              => 0x08,
#    FLAG_SOURCE_LINES           => 0x10,
#    FLAG_SINGLE_STEP_ON         => 0x20,
#    FLAG_USE_SUB_ADDRESS        => 0x40,
#    FLAG_REPORT_GOTO            => 0x80,
#    FLAG_EVAL_FILENAMES         => 0x100,
#    FLAG_ANON_FILENAMES         => 0x200,
#    FLAG_STORE_SOURCES          => 0x400,
#    FLAG_KEEP_SUBLESS_EVALS     => 0x800,
#    FLAG_KEEP_UNCOMPILED_SOURCE => 0x1000,
#    FLAGS_DESCRIPTIVE_NAMES     => [
#        q{Debug subroutine enter/exit.},
#        q{Line-by-line debugging. Causes DB::DB() subroutine to be called for each statement executed. Also causes saving source code lines (like 0x400).}
#        ,
#        q{Switch off optimizations.},
#        q{Preserve more data for future interactive inspections.},
#        q{Keep info about source lines on which a subroutine is defined.},
#        q{Start with single-step on.},
#        q{Use subroutine address instead of name when reporting.},
#        q{Report goto &subroutine as well.},
#        q{Provide informative "file" names for evals based on the place they were compiled.},
#        q{Provide informative names to anonymous subroutines based on the place they were compiled.},
#        q{Save source code lines into @{"::_<$filename"}.},
#        q{When saving source, include evals that generate no subroutines.},
#        q{When saving source, include source that did not compile.},
#    ],
#};

sub FLAG_REPORT_GOTO() {0x80;}

sub STEP_CONTINUE() {0;}
sub STEP_INTO() {1;}
sub STEP_OVER() {2;}



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

my $coder;  # JSON::XS coder

my $frame_prefix_step = "  ";
my $frame_prefix = '';

my $_internal_process = 0;

my @saved;  # saved runtime environment

my $current_package;
my $current_file;
my $current_line;
my $current_sub;
my $current_hasargs;
my $current_wantarray;
my $current_evaltext;
my $current_is_require;
my $current_hints;
my $current_bitmask;
my $current_hinthash;

my $trace_set_db_line = 0; # report _set_dbline invocation

my $_stack_frames = [ ];     # stack frames

sub _dump
{
    require Data::Dumper;
    return Data::Dumper->Dump( [ @_ ] );
}

sub _report($;@)
{
    my ($message, @sprintf_args) = @_;
    chomp $message;
    printf STDERR "$message\n", @sprintf_args;
}

sub _render_variables
{
    my ($vars_hash) = @_;
    my $result = '';

    require Scalar::Util;

    foreach my $variable (keys %$vars_hash)
    {
        my $value = $vars_hash->{$variable};

        my $reftype = Scalar::Util::reftype $value;
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

sub _send_to_debugger
{
    my ($string) = @_;
    $string .= "\n";
    print $_debug_socket $string;
    #    print STDERR "* Sent to debugger: $string";
}

sub _get_adjusted_line_number
{
    my ($filename, $line_number) = @_;
    $line_number--;
    return $line_number;
}

sub _serialize
{
    my ($data) = @_;
    unless ($coder)
    {
        require JSON::XS;
        $coder = JSON::XS->new();
        $coder->latin1();
    }
    return $coder->encode( $data );
}

sub _get_stop_command
{
    my $data = {
        event  => 'STOP',
        frames => [
        ],
    };

    my $frames = $data->{frames};

    foreach my $stack_frame (@{$_stack_frames})
    {
        my $new_frame = {
            name => "$stack_frame->{subname}",
            file => $_real_filenames{$stack_frame->{file}},
            line => _get_adjusted_line_number( $stack_frame->{file}, $stack_frame->{current_line} ),
        };

        # fixme handle Step at main:: (eval 25)[C:\Repository\IDEA-Perl5-Debugger\testscript.pl:40] line 2 with , 0-0-2, depth 1

        if (!defined $new_frame->{file})
        {
            _report( "Couldn't find real filename for %s, %s", _dump( $stack_frame ), _dump( \%_real_filenames ) );
            $new_frame->{file} = $stack_frame->{file};
        }

        $new_frame->{file} =~ s{\\}{/};

        push @$frames, $new_frame;
    }

    return _serialize( $data );
}

sub _event_handler
{
    _send_to_debugger( _get_stop_command() );
    while()
    {
        my $command = <$_debug_socket>;
        die 'Debugging socket disconnected' if !defined $command;
        $command =~ s/[\r\n]+$//;
        print STDERR "============> Got command: '$command'\n";

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
            print STDERR _dump( \%DB::sub );
        }
        elsif ($command =~ /^e\s+(.+)$/) # eval expresion
        {
            my @lsaved = ($@, $!, $,, $/, $\, $^W);
            my $expr = "package $current_package;$1";
            print STDERR "Running $expr\n";
            ($@, $!, $,, $/, $\, $^W) = @saved;
            my $result = eval $expr;
            ($@, $!, $,, $/, $\, $^W) = @lsaved;
            print STDERR "Result is $result\n";
        }
        elsif ($command eq 'f') # dump keys of %DB::dbline
        {
            print STDERR _dump( \%_real_filenames );
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
            print _dump( \%DB::dbline );
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
            $DB::single = STEP_OVER;
            return;
        }
        elsif ($command eq 'u') # step out
        {
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
                    $frame->{file},
                    $frame->{from_line}
                );

                _report( "    * Lexical variables:\n%s", _render_variables( $frame->{lexical_vars} ) );
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
    ($current_package,
        $current_file,
        $current_line,
        $current_sub,
        $current_hasargs,
        $current_wantarray,
        $current_evaltext,
        $current_is_require,
        $current_hints,
        $current_bitmask,
        $current_hinthash
    ) = caller( 1 );

    print STDERR $frame_prefix.'Unknown caller' if !defined $current_line;

    _report( <<'EOM',
%sCaller: %s %s::%s%s in %s, %s, ev:%s; s:%s w:%s %s-%s-%s
EOM
        $frame_prefix,
            defined $current_wantarray ? $current_wantarray ? 'array' : 'scalar' : 'void',
        $current_package // 'undef',
        $current_sub // 'undef',
            $current_is_require ? '(require)' : '',
        $current_file // 'undef',
        $current_line // 'undef',
        $current_evaltext // 'undef',
        $current_hints // '', # strict
        $current_bitmask // '', # warnings
        $current_hasargs // 'undef',
        $current_hinthash // 'undef',
        ${^GLOBAL_PHASE} // 'unknown',
    );

    no strict 'refs';
    *DB::dbline = *{"::_<$current_file"};
}

sub _update_frame_position
{
    my $current_stack_frame = _get_current_stack_frame();
    $current_stack_frame->{current_line} = $current_line;
    $current_stack_frame->{file} = $current_file;
    $current_stack_frame->{lexical_vars} = peek_my( 2 );
    $_real_filenames{$current_file} //= _get_real_path( $current_file, $current_file );
}

sub _get_current_stack_frame
{
    return $_stack_frames->[0];
}

sub _enter_frame
{
    my ($args_ref, $old_db_single) = @_;
    _update_frame_position();

    _report $frame_prefix."Entering frame %s: %s%s %s-%s-%s",
        scalar @$_stack_frames + 1,
        $DB::sub,
            scalar @$args_ref ? '('.(join ', ', @$args_ref).')' : '()',
        $DB::trace // 'undef',
        $DB::signal // 'undef',
        $old_db_single // 'undef',
    ;
    $frame_prefix = $frame_prefix_step x (scalar @$_stack_frames + 1);

    my $sub_file = '';
    my $sub_line = 0;

    if ($DB::sub{$DB::sub})
    {
        if ($DB::sub{$DB::sub} =~ /^(.+?):(\d+)-(\d+)$/)
        {
            $sub_file = $1;
            $sub_line = $2;
        }
        else
        {
            _report( $frame_prefix."  * Unable to parse sub data for %s, %s", $DB::sub, $DB::sub{$DB::sub} );
        }
    }
    else
    {
        _report( $frame_prefix."  * Unable to find file data for %s, %s", $DB::sub, "" #join ', ', keys %DB::sub
        );
    }

    my $new_stack_frame = {
        subname      => $DB::sub,
        args         => $args_ref,
        file         => $sub_file,
        current_line => $sub_line,
        _single      => $old_db_single,
        _dbline      => *DB::dbline
    };
    unshift @$_stack_frames, $new_stack_frame;
    ($@, $!, $,, $/, $\, $^W) = @saved;
    return $new_stack_frame;
}


sub _exit_frame
{
    @saved = ($@, $!, $,, $/, $\, $^W);
    my $frame = shift @$_stack_frames;
    $frame_prefix = $frame_prefix_step x (scalar @$_stack_frames);
    _report $frame_prefix."Leaving frame %s, setting single to %s", (scalar @$_stack_frames + 1), $frame->{_single};
    $DB::single = $frame->{_single};
    *DB::dbline = $frame->{_dbline};
    ($@, $!, $,, $/, $\, $^W) = @saved;
}

sub _get_real_path
{
    my $path = shift;
    my $new_filename = shift;

    my $real_path;

    if ($path !~ m{^(/|\w\:)})
    {
        print STDERR $frame_prefix."Detecting path for $path\n";

        my $current_dir = Cwd::getcwd();

        $real_path = "$current_dir/$path";
    }
    else
    {
        $real_path = $path;
    }
    print STDERR $frame_prefix."  * $new_filename real path is $real_path\n";
    return $real_path;
}


# When the execution of your program reaches a point that can hold a breakpoint, the DB::DB() subroutine is called if
# any of the variables $DB::trace , $DB::single , or $DB::signal is true. These variables are not localizable. This
# feature is disabled when executing inside DB::DB() , including functions called from it unless $^D & (1<<30) is true.
sub step_handler
{
    return if $_internal_process;
    $_internal_process = 1;

    my $old_db_single = $DB::single;
    $DB::single = STEP_CONTINUE;
    @saved = ($@, $!, $,, $/, $\, $^W);

    print STDERR $frame_prefix."Set dbline from step handler\n" if $trace_set_db_line;
    _set_dbline();
    _update_frame_position();

    _report $frame_prefix."Step with %s, %s-%s-%s",
        (join ',', @_) // '',
        $DB::trace // 'undef',
        $DB::signal // 'undef',
        $old_db_single // 'undef',
    ;

    print STDERR $DB::dbline[$current_line];

    _event_handler( );

    ($@, $!, $,, $/, $\, $^W) = @saved;
    $_internal_process = 0;
    return;
}


# this pass-through flag handles quotation overload loop
sub sub_handler
{
    @saved = ($@, $!, $,, $/, $\, $^W);
    my $stack_frame;

    my $old_db_single = $DB::single;
    if (!$_internal_process)
    {
        $_internal_process = 1;

        $DB::single = STEP_CONTINUE;

        print STDERR $frame_prefix."Set dbline from sub handler $DB::sub\n" if $trace_set_db_line;
        _set_dbline();
        $stack_frame = _enter_frame( [ @_ ], $old_db_single );

        if ($current_package && $current_package eq 'DB')
        {
            printf STDERR "    * Second frame for %s is %s\n", $DB::sub, join ', ', map $_ // 'undef', caller( 1 );
        }

        $DB::single = $old_db_single;
        $_internal_process = 0;
    }

    my $wantarray = wantarray;

    if ($DB::single == STEP_OVER)
    {
        print STDERR $frame_prefix."Disabling step in in subcalls\n";
        $DB::single = STEP_CONTINUE;
    }
    else
    {
        printf STDERR $frame_prefix."Keeping step as %s\n", $old_db_single if $stack_frame;
    }

    if ($DB::sub eq 'DESTROY' or substr( $DB::sub, -9 ) eq '::DESTROY' or !defined $wantarray)
    {
        no strict 'refs';
        ($@, $!, $,, $/, $\, $^W) = @saved;
        &$DB::sub;

        if ($stack_frame)
        {
            _exit_frame();
        }
        else
        {
            $DB::single = $old_db_single;
        }

        return $DB::ret = undef;
    }
    if ($wantarray)
    {
        no strict 'refs';
        ($@, $!, $,, $/, $\, $^W) = @saved;
        my @result = &$DB::sub;
        if ($stack_frame)
        {
            _exit_frame();
        }
        else
        {
            $DB::single = $old_db_single;
        }
        return @DB::ret = @result;
    }
    else
    {
        no strict 'refs';
        ($@, $!, $,, $/, $\, $^W) = @saved;
        my $result = &$DB::sub;
        if ($stack_frame)
        {
            _exit_frame();
        }
        else
        {
            $DB::single = $old_db_single;
        }
        return $DB::ret = $result;
    }
}

# If the call is to an lvalue subroutine, and &DB::lsub is defined &DB::lsub (args) is called instead, otherwise
# falling back to &DB::sub (args).
sub lsub_handler: lvalue
{
    @saved = ($@, $!, $,, $/, $\, $^W);
    my $stack_frame;

    my $old_db_single = $DB::single;
    if (!$_internal_process)
    {
        $_internal_process = 1;

        $DB::single = STEP_CONTINUE;
        print STDERR $frame_prefix."Set dbline from lsub handler\n" if $trace_set_db_line;
        _set_dbline();
        $stack_frame = _enter_frame( [ @_ ], $old_db_single );

        $DB::single = $old_db_single;
        $_internal_process = 0;
    }

    if ($DB::single == STEP_OVER)
    {
        print STDERR $frame_prefix."Disabling step in in subcalls\n";
        $DB::single = STEP_CONTINUE;
    }
    else
    {
        printf STDERR $frame_prefix."Keeping step as %s\n", $old_db_single if $stack_frame;
    }

    {
        no strict 'refs';
        ($@, $!, $,, $/, $\, $^W) = @saved;
        my $result = &$DB::sub;
        if ($stack_frame)
        {
            _exit_frame();
        }
        else
        {
            $DB::single = $old_db_single;
        }
        return $DB::ret = $result;
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
    @saved = ($@, $!, $,, $/, $\, $^W);

    if (!$_internal_process)
    {
        $_internal_process = 1;
        print STDERR $frame_prefix."Set dbline from load handler\n" if $trace_set_db_line;
        _set_dbline();
        _update_frame_position();
        $_internal_process = 0;
    }

    _report $frame_prefix."Loading module: %s %s-%s-%s",
        $_[0],
        $DB::trace // 'undef',
        $DB::signal // 'undef',
        $old_db_single // 'undef',
    ;

    my $new_filename = $_[0];
    if ($new_filename =~ /_<(.+)$/)
    {
        my $path = $1;
        $_real_filenames{$path} = _get_real_path( $path, $new_filename );
    }
    else
    {
        die "Incorrect file: $new_filename";
    }

    ($@, $!, $,, $/, $\, $^W) = @saved;
    $DB::single = $old_db_single;
}
# When execution of the program uses goto to enter a non-XS subroutine and the 0x80 bit is set in $^P , a call to
# &DB::goto is made, with $DB::sub holding the name of the subroutine being entered.
sub goto_handler
{
    return if $_internal_process;
    $_internal_process = 1;

    my $old_db_single = $DB::single;
    $DB::single = STEP_CONTINUE;

    @saved = ($@, $!, $,, $/, $\, $^W);
    print STDERR $frame_prefix."Set dbline from goto handler\n" if $trace_set_db_line;
    _set_dbline();
    _update_frame_position();

    _report $frame_prefix."Goto called%s from %s:: %s line %s %s-%s-%s-%s",
                scalar @_ ? ' with '.(join ',', @_) : '',
            $current_package // 'undef',
            $current_file // 'undef',
            $current_line // 'undef',
            $DB::trace // 'undef',
            $DB::signal // 'undef',
            $old_db_single // 'undef',
        ${^GLOBAL_PHASE} // 'unknown',
        ;
    ($@, $!, $,, $/, $\, $^W) = @saved;
    $DB::single = $old_db_single;
    $_internal_process = 0;
}


$_local_debug_host = 'localhost';
$_local_debug_port = 12345;
$_debug_net_role = 'server';

$^P |= FLAG_REPORT_GOTO;

# http://perldoc.perl.org/perlipc.html#Sockets%3a-Client%2fServer-Communication
if ($_debug_net_role eq 'server')
{
    _report( "Listening for the debugger at %s:%s...", $_local_debug_host, $_local_debug_port );
    my $_server_socket = IO::Socket::INET->new(
        Listen    => 1,
        LocalAddr => $_local_debug_host,
        LocalPort => $_local_debug_port,
        ReuseAddr => 1,
        Proto     => 'tcp',
    ) || die "Error binding to $_local_debug_host:$_local_debug_port";
    $_debug_packed_address = accept( $_debug_socket, $_server_socket );
}
else
{
    _report( "Connecting to the debugger at %s:%s...", $_debug_server_host, $_debug_server_port );
    $_debug_socket = IO::Socket::INET->new(
        PeerAddr  => $_debug_server_host,
        LocalPort => $_debug_server_port,
        ReuseAddr => 1,
        Proto     => 'tcp',
    ) || die "Error connecting to $_debug_server_host:$_debug_server_port";
}
$_debug_socket->autoflush( 1 );

print STDERR $frame_prefix."Set dbline from main\n" if $trace_set_db_line;
_set_dbline();

push @$_stack_frames, {
        subname      => 'SCRIPT',
        args         => [ @ARGV ],
        file         => $current_file,
        current_line => $current_line,
        _single      => STEP_INTO,
        _dbline      => *DB::dbline,
    };

*DB::DB = \&step_handler;
*DB::sub = \&sub_handler;
*DB::lsub = \&lsub_handler;
*DB::postponed = \&load_handler;
*DB::goto = \&goto_handler;

require Cwd;
Cwd::getcwd();

$_real_filenames{$current_file} = _get_real_path( $current_file, $current_file );
$frame_prefix = $frame_prefix_step;

$DB::single = STEP_INTO;

#$DB::single = STEP_CONTINUE;

1; # End of Devel::Camelcadedb
