package Devel::Camelcadedb;
# must be quoted to work correctly with JSON protocol
our $VERSION = "2017.100.3";

# http://perldoc.perl.org/DB.html
# http://perldoc.perl.org/perldebug.html
# http://perldoc.perl.org/perldebtut.html
# http://perldoc.perl.org/perldebguts.html
package DB;
use 5.008;
use strict;
use warnings;
use IO::Socket::INET;
use IO::Select;
use PadWalker qw/peek_my peek_our/;
use Scalar::Util;
use Encode;
use overload;
use PerlIO;
use Hash::StoredIterator;
#use Carp;

#sub FLAG_REPORT_GOTO() {0x80;}

use constant {
    STEP_CONTINUE => 0,
    STEP_INTO     => 1,
    STEP_OVER     => 2,
};

use constant {
    # see PERLDBf_* constants in perl.h
    DEBUG_ALL             => 0x7ff,
    DEBUG_SINGLE_STEP_ON  => 0x20,
    DEBUG_USE_SUB_ADDRESS => 0x40,
    DEBUG_REPORT_GOTO     => 0x80,
};
use constant {
    # debugger enabled
    DEBUG_DEFAULT_FLAGS => DEBUG_ALL & ~(DEBUG_USE_SUB_ADDRESS|DEBUG_REPORT_GOTO), # 0x73f
    # instrument code, but don't call DB::DB (see sub disable for DB::sub)
    DEBUG_PREPARE_FLAGS => DEBUG_ALL & ~(DEBUG_USE_SUB_ADDRESS|DEBUG_REPORT_GOTO|DEBUG_SINGLE_STEP_ON), # 0x73c
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

my %_perl_file_id_to_path_map = ();  # map of perl file ids without _<  => real path detected on loading
my %_paths_to_perl_file_id_map = (); # maps real paths to _<filename

my %_loaded_breakpoints = (); # map of loaded breakpoints, set and not in form: path => line => object
my %_queued_breakpoints_files = (); # map of files with loaded and not set breakpoints
my %_references_cache = ();   # cache of soft references from peek_my

my %_source_been_sent = (); # flags that source been sent
my %_file_name_sent = ();   # flags that idea been notfied about this file loading
my %_evals_to_templates_map = ();        # map of evals to templates or something (see template_handler). Structure: eval => target file
my %_templates_to_evals_map = ();        # map of templates to evals or something (see template_handler). Structure: template => [eval1, eval2, ...]

my @glob_slots = qw/SCALAR ARRAY HASH CODE IO FORMAT/;
my $glob_slots = join '|', @glob_slots;

my $_dev_mode = 0;                              # enable this to get verbose STDERR output from process
my $_debug_log_fh = *STDERR;                    # debug log fh. If omited, file will be created
my $_debug_log_filename = 'current_debug.log';
my $_debug_sub_handler = 1;                     # debug entering/leaving subs, works in dev mode
my $_debug_load_handler = 0;                    # debug modules loading
my $_debug_breakpoints = 0;                     # debug breakpoints setting

my $_script_charset = 'utf8';   # all sources and strings without utf flag will be encoded from this encoding to the utf

my $_skip_run_stop = 0; # flag for skipping forced stop on run phase

# this enables pausing and breakpoints setting while script is running, gives moderate overhead
my $_enable_noninteractive_mode = 0;

# this enables attemp to set a breakpoint on each leaving/entering sub, gives large overhead, proportional number of breakpoints
my $_enable_compile_time_breakpoints = 0;

my $_debug_socket;
my $_debug_packed_address;
my IO::Select $_debug_socket_select;

my $coder;  # JSON::XS coder
my $deparser; # B::Deparse deparser

my $frame_prefix_step = "  ";
my $frame_prefix = '';

my $_internal_process = 0;

my @saved;  # saved runtime environment

my $current_package;
my $current_file_id;
my $current_line;

my $trace_code_stack_and_frames = 0; # report traces on entering code
my $trace_real_path = 0; # trasing real path transition

my $ready_to_go = 0; # set after debugger been initialized

my $_stack_frames = [ ];     # stack frames

sub _report($;@)
{
    return unless $_dev_mode;
    my ($message, @sprintf_args) = @_;
    chomp $message;

    unless ($_debug_log_fh)
    {
        open $_debug_log_fh, ">", $_debug_log_filename or die "Unable to open debug log $_debug_log_filename $!";
        $_debug_log_fh->autoflush( 1 );
    }

    printf $_debug_log_fh "$frame_prefix$message\n", map {$_ // 'undef'} @sprintf_args;
}

sub _format_caller
{
    my (@caller) = @_;
    return sprintf "%s %s%s%s from %s::, %s line %s; %s %s %s %s",
        map $_ // 'undef',
                defined $caller[5] ? $caller[5] ? 'array' : 'scalar' : 'void', # wantarray
            $caller[3], # target sub
                $caller[4] ? '(@_)' : '', # has args
                $caller[7] ? ' [require '.$caller[6].']' : '', # is_require and evaltext
            $caller[0], # package
            $caller[1], # filename
            $caller[2], # line
                $caller[7] ? '' : $caller[6] // '', # evaltext if no isrequire
            $caller[8], # strcit
            $caller[9], # warnings
            $caller[10], # hinthash
    ;
}

sub _get_loaded_files_map
{
    my %result = ();
    foreach my $key (keys %::)
    {
        my $glob = $::{$key};
        next unless $key =~ s/^_<//;
        next unless *$glob{ARRAY} && scalar @{*$glob{ARRAY}};
        $result{$key} = ${*$glob};
    }
    return \%result;
}

sub _get_file_descriptor_by_id
{
    my ($file_id) = @_;

    my $real_path = _get_real_path_by_normalized_perl_file_id( $file_id );
    my $presentable_name;

    if ($real_path =~ /^\(eval \d+\)/)
    {
        my $eval_map_entry = $_evals_to_templates_map{$real_path};
        if ($eval_map_entry && $eval_map_entry->{path})
        {
            $presentable_name = $eval_map_entry->{path};
        }
        #        else
        #        {
        #            $presentable_name = $real_path;
        #            $presentable_name =~ s/^(\(eval \d+\)).+$/$1/;
        #        }
    }

    return {
        path => $real_path,
        name => $presentable_name,
    };
}

sub _send_loaded_files_names
{
    my $loaded_files_map = _get_loaded_files_map();
    my @files_to_add = ();
    my @files_to_remove = ();

    foreach my $file_id (keys %$loaded_files_map)
    {
        next if index( $file_id, 'Camelcadedb.pm' ) != -1 || exists $_file_name_sent{$file_id};
        $_file_name_sent{$file_id} = 1;
        push @files_to_add, _get_file_descriptor_by_id( $file_id );
    }

    foreach my $file_id (keys %_file_name_sent)
    {
        next if exists $loaded_files_map->{$file_id};
        delete $_file_name_sent{$file_id};
        push @files_to_remove, _get_file_descriptor_by_id( $file_id );
    }

    if (scalar @files_to_add + scalar @files_to_remove)
    {
        _send_event( "LOADED_FILES_DELTA", {
                add    => \@files_to_add,
                remove => \@files_to_remove
            } );
    }
}


sub _send_breakpoint_reached_event
{
    my ($breakpoint) = @_;

    my $event_data = {
        path       => $breakpoint->{path},
        line       => $breakpoint->{line} - 1,
        logmessage => $breakpoint->{action_result},
    };

    if ($breakpoint->{suspend})
    {
        $event_data->{suspend} = \1;
        $event_data->{frames} = _calc_stack_frames();
    }
    else
    {
        $event_data->{suspend} = \0;
        $event_data->{frames} = [ ];
    }

    _send_event( 'BREAKPOINT_REACHED', $event_data );
}

sub _send_event
{
    my ($name, $data) = @_;

    _send_data_to_debugger( +{
            event => $name,
            data  => $data
        } );
}

sub _dump_stack
{
    my $depth = 0;
    _report "Stack trace:\n" if $_dev_mode;
    while()
    {
        my @caller = caller( $depth );
        last unless defined $caller[2];
        _report $frame_prefix_step."%s: %s\n", $depth++, _format_caller( @caller ) if $_dev_mode;
    }
    1;
}

sub _dump_frames
{
    my $depth = 0;
    _report "Frames trace:\n" if $_dev_mode;
    foreach my $frame (@$_stack_frames)
    {
        _report $frame_prefix_step."%s: %s\n", $depth++,
            join ', ', map $_ // 'undef', @$frame{qw/subname file current_line single/},
                        $frame->{is_use_block} ? '(use block)' : ''
            if $_dev_mode;
    }
    1;
}

sub _deparse_code
{
    my ($code) = @_;
    $deparser ||= B::Deparse->new();
    return $deparser->coderef2text( $code );
}

sub _send_transaction_response
{
    my ($transaction_id, $data) = @_;

    _send_data_to_debugger( +{
            event         => 'RESPONSE',
            transactionId => $transaction_id,
            data          => $data,
        }
    );
}

sub _get_file_source_by_file_id
{
    my ($file_id) = @_;
    $_source_been_sent{$file_id} = 1;
    {
        no strict 'refs';
        _report "Getting source of main::_<$file_id" if $_dev_mode;
        my @lines = @{"main::_<$file_id"};
        shift @lines;
        return _to_utf8( join '', @lines );
    }
}

sub _get_file_source_once_by_file_id
{
    my ($file_id) = @_;
    return if $_source_been_sent{$file_id};
    return _get_file_source_by_file_id( $file_id );
}

sub _get_file_source_handler
{
    my ($request_serialized_object) = @_;
    my $transaction_wrapper = _deserialize( $request_serialized_object );
    my ($transaction_id, $request_object) = @$transaction_wrapper{qw/id data/};

    my $file_id = _get_perl_file_id_by_real_path( $request_object->{path} );

    _report "Fetching source for $file_id $request_object->{path}" if $_dev_mode;

    _send_transaction_response(
        $transaction_id,
        _get_file_source_once_by_file_id( $file_id ) // '# No source found for '.$file_id
    );
}

sub _get_reference_subelements
{
    my ($request_serialized_object) = @_;
    my $transaction_wrapper = _deserialize( $request_serialized_object );
    my ($transaction_id, $request_object) = @$transaction_wrapper{qw/id data/};
    my ($offset, $size, $key) = @$request_object{qw/offset limit key/};
    my $data = [ ];

    my $source_data;

    if ($key =~ /^\*(.+?)(?:\{($glob_slots)\})?$/) # hack for globs by names
    {
        no strict 'refs';
        my ( $name, $slot) = ($1, $2);

        if ($slot)
        {
            $source_data = *{$name}{$slot};
        }
        else
        {
            $source_data = \*{$name};
        }

        _report "Got glob ref $key => $source_data" if $_dev_mode;
    }
    else
    {
        $source_data = $_references_cache{$key};
    }
    if ($source_data)
    {
        my $reftype = Scalar::Util::reftype( $source_data );

        if ($reftype eq 'ARRAY' && $#$source_data >= $offset)
        {
            my $last_index = $offset + $size;

            for (my $item_number = $offset; $item_number < $last_index && $item_number < @$source_data; $item_number++)
            {
                push @$data, _get_reference_descriptor( "[$item_number]", \$source_data->[$item_number] );
            }
        }
        elsif ($reftype eq 'HASH')
        {
            my $hash_iterator = Hash::StoredIterator::hash_get_iterator( $source_data );
            my @keys = sort keys %$source_data;
            Hash::StoredIterator::hash_set_iterator( $source_data, $hash_iterator );

            if ($#keys >= $offset)
            {
                my $last_index = $offset + $size;

                for (my $item_number = $offset; $item_number < $last_index && $item_number < @keys; $item_number++)
                {
                    my $hash_key = $keys[$item_number];
                    push @$data, _get_reference_descriptor( "'$hash_key'", \$source_data->{$hash_key} );
                }
            }
        }
        elsif ($reftype eq 'REF')
        {
            push @$data, _get_reference_descriptor( $source_data, \$$source_data );
        }
        elsif ($reftype eq 'GLOB')
        {
            no strict 'refs';

            foreach my $glob_slot (@glob_slots)
            {
                my $reference = *$source_data{$glob_slot};
                next unless $reference;
                my $desciptor = _get_reference_descriptor( $glob_slot, \$reference );

                # hack for DB namespace, see https://github.com/hurricup/Perl5-IDEA/issues/1151
                if ($glob_slot eq 'HASH' && $key =~ /^\*(::)*(main::)*(::)*DB(::)?$/)
                {
                    $desciptor->{expandable} = \0;
                    $desciptor->{size} = 0;
                }

                push @$data, $desciptor;
            }

        }
        else
        {
            _report "Dont know how to iterate $reftype" if $_dev_mode;
        }

    }
    else
    {
        _report "No source data for $key\n" if $_dev_mode;
    }

    _send_transaction_response( $transaction_id, $data );
}

sub _format_variables_hash
{
    my ($vars_hash) = @_;

    my $result = [ ];

    foreach my $variable (sort keys %$vars_hash)
    {
        my $value = $vars_hash->{$variable};
        push @$result, _get_reference_descriptor( $variable, $value );
    }

    return $result;
}

sub _to_utf8
{
    my ($value) = @_;

    return $value unless $value;

    if (utf8::is_utf8( $value )) # if values is marked as utf8
    {
        utf8::encode( $value ); # we just making octets from it
    }
    elsif ($value =~ /[\x80-\xFF]/) # otherwise, if we've got non-ascii symbols, we suppose it's in configured encoding
    {
        Encode::from_to( $value, $_script_charset, 'utf8' );
    }

    return $value;
}

sub _from_utf8
{
    my ($value) = @_;

    return $value unless $value;

    if ($_script_charset ne 'utf8') # if script uses non-utf encoding, just encode data to the script encoding
    {
        Encode::from_to( $value, 'utf8', $_script_charset );
    }
    else # otherwise, decode octets to characters
    {
        utf8::decode( $value );
    }

    return $value;
}

sub _get_reference_descriptor
{
    my ($name, $value) = @_;

    my $key = $value;
    my $reftype = Scalar::Util::reftype( $value );
    my $ref = ref $value;

    my $size = 0;
    my $type = overload::StrVal( $value );

    my $expandable = \0;
    my $is_blessed = $ref && Scalar::Util::blessed( $value ) ? \1 : \0;
    my $ref_depth = 0;
    my $is_utf = \0;
    my $layers = undef;

    my $tied;

    if (!$reftype)
    {
        $type = "SCALAR";
        $tied = tied $value;
        $is_utf = defined $value && utf8::is_utf8( $value ) ? \1 : \0;
        $value = defined $value ? "\"$value\"" : 'undef'; #_escape_scalar(
        $key //= 'undef';
    }
    elsif ($reftype eq 'SCALAR')
    {
        $tied = tied $$value;
        $is_utf = defined $$value && utf8::is_utf8( $$value ) ? \1 : \0;
        $value = defined $$value ? "\"$$value\"" : 'undef'; #_escape_scalar(
    }
    elsif ($reftype eq 'REF')
    {
        my $result = _get_reference_descriptor( $name, $$value );
        $result->{ref_depth}++;
        return $result;
    }
    elsif ($reftype eq 'ARRAY')
    {
        $size = scalar @$value;
        $tied = tied @$value;
        $value = sprintf "size = %s", $size;
        $expandable = $size ? \1 : \0;
    }
    elsif ($reftype eq 'HASH')
    {
        $tied = tied %$value;
        my $hash_iterator = Hash::StoredIterator::hash_get_iterator( $value );
        $size = scalar keys %$value;
        Hash::StoredIterator::hash_set_iterator( $value, $hash_iterator );

        $value = sprintf "size = %s", $size;
        $expandable = $size ? \1 : \0;
    }
    elsif ($reftype eq 'GLOB')
    {
        no strict 'refs';
        $tied = tied *$value;
        $size = scalar grep *$value{$_}, @glob_slots;
        $value = "*".*$value{PACKAGE}."::".*$value{NAME};
        $layers = _get_layers($key);
        $expandable = $size ? \1 : \0;
    }

    my $char_code;
    my $stringified_key = "$key";
    $stringified_key =~ s{(.)}{
        $char_code = ord( $1 );
        $char_code < 32 ? '^'.chr( $char_code + 0x40 ) : $1
        }gsex;

    if ($reftype)
    {
        $_references_cache{$stringified_key} = $key;
    }

    $name = "$name";
    $value = "$value";

    $name =~ s{(.)}{
        $char_code = ord( $1 );
        $char_code < 32 ? '^'.chr( $char_code + 0x40 ) : $1
        }gsex;
    $value =~ s{([^\n\r\f\t])}{
        $char_code = ord( $1 );
        $char_code < 32 ? '^'.chr( $char_code + 0x40 ) : $1
        }gsex;

    # handling encoding

    my $result = {
        name       => _to_utf8( "$name" ),
        value      => _to_utf8( "$value" ),
        type       => "$type",
        expandable => $expandable,
        key        => $stringified_key,
        size       => $size,
        blessed    => $is_blessed,
        ref_depth  => $ref_depth,
        is_utf     => $is_utf
    };

    $result->{layers} = $layers if $layers;
    $result->{tied_with} = _get_reference_descriptor(object => $tied) if $tied;

    return $result;
}

sub _get_layers{
    my $glob = shift;

    my %result = ();
    my $input_layers = _pack_layers(PerlIO::get_layers($glob, details => 1));
    $result{input} = $input_layers if $input_layers && @$input_layers;

    my $output_layers = _pack_layers(PerlIO::get_layers($glob, details => 1, output=>1));
    $result{output} = $output_layers if $output_layers && @$output_layers;

    return scalar keys %result ? \%result: undef;
}

sub _pack_layers{
    my @result = ();
    push @result, { name => shift, param => shift, flags => shift} while @_;
    return \@result;
}

#
# Making scalar control elements visible, \n\r for now, need cool conception
#
my %map = (
    "\n" => '\n',
    "\r" => '\r',
    "\f" => '\f',
    "\t" => '\t',
);

sub _escape_scalar
{
    my ($scalar) = @_;
    $scalar =~ s{\\(?=[rnft])}{\\\\}sg;
    $scalar =~ s/([\r\n\f\t])/$map{$1}/seg;
    return $scalar;
}

sub _get_current_stack_frame
{
    return $_stack_frames->[-1];
}

sub _send_data_to_debugger
{
    my ($event) = @_;
    _send_string_to_debugger( _serialize( $event ) );
}

sub _send_string_to_debugger
{
    my ($string) = @_;
    $string .= "\n";
    print $_debug_socket $string;
    _report "Sent to debugger: %s", $string if $_dev_mode;
}

sub _get_adjusted_line_number
{
    my ($line_number) = @_;
    return $line_number - 1;
}

#@returns JSON::XS
sub _get_seraizlier
{
    unless ($coder)
    {
        $coder = JSON::XS->new();
        $coder->latin1();
    }
    return $coder;
}

sub _serialize
{
    my ($data) = @_;
    return _get_seraizlier->encode( $data );
}

sub _deserialize
{
    my ($json_string) = @_;
    return _get_seraizlier->decode( $json_string );
}

sub _calc_stack_frames
{
    my $frames = [ ];
    my $depth = 0;
    %_references_cache = ();

    while ()
    {
        my ($package, $filename, $line, $subroutine, $hasargs,
            $wantarray, $evaltext, $is_require, $hints, $bitmask, $hinthash) = caller( $depth );

        my $cnt = 0;
        my @frame_args = map _get_reference_descriptor( '$_['.$cnt++.']', $_ ), @DB::args;

        last unless defined $filename;

        if ($package && $package ne 'DB')
        {
            if (@$frames && $subroutine ne '(eval)')
            {
                $frames->[-1]->{file}->{name} = $subroutine;
            }

            my $global_variables = [ ];
            my $global_variables_hash = eval {peek_our( $depth + 1 )};
            unless ($@)
            {
                $global_variables = _format_variables_hash( $global_variables_hash );
            }

            my $lexical_variables = [ ];
            my $variables_hash = eval {peek_my( $depth + 1 )};
            unless ($@)
            {
                $lexical_variables = _format_variables_hash( $variables_hash );
            }

            $frames->[-1]->{args} = \@frame_args if scalar @$frames;

            my $descriptor = _get_file_descriptor_by_id( $filename );

            push @$frames, {
                    file      => $descriptor,
                    line      => $line - 1,
                    lexicals  => $lexical_variables,
                    globals   => $global_variables,
                    main_size => scalar keys %::,
                    args      => [ ],
                };
        }

        $depth++;
    }

    return $frames;
}

sub _is_use_frame
{
    my $stack_frame = shift;
    my $is_use_block = 0;

    if (ref $stack_frame->{subname})
    {
        my $deparsed_block = _deparse_code( $stack_frame->{subname} );
        $is_use_block = $deparsed_block =~ /require\s+[\w\:]+\s*;\s*do/si;
    }

    return $is_use_block;
}

sub _set_frames_single
{
    my ($new_value) = @_;
    foreach my $frame (@{$_stack_frames})
    {
        $frame->{single} = $new_value;
    }
    $DB::single = $new_value;
}

sub _hold_the_line
{
    _set_frames_single( STEP_INTO );
}

sub _release_the_hounds
{
    _set_frames_single( STEP_CONTINUE );
}

sub _process_command
{
    my ($command) = @_;
    _report "============> Got command: '%s'\n", $command if $_dev_mode;

    if ($command eq 'q')
    {
        _report "Exiting" if $_dev_mode;
        exit;
    }
    elsif ($command =~ /^e\s+(.+)$/) # eval expresion
    {
        my $data = $1;
        my $transaction_data = _deserialize( $data );

        my ($transaction_id, $request_object) = @$transaction_data{qw/id data/};

        my $result = _eval_expression( _from_utf8( $request_object->{expression} // '' ) );
        $result->{result} = _get_reference_descriptor( result => $result->{result} );

        _send_transaction_response( $transaction_id, $result );

        _report "Result is $result\n" if $_dev_mode;
    }
    elsif ($command eq 'pause')
    {
        _hold_the_line;
    }
    elsif ($command eq 'g')
    {
        _release_the_hounds();
        return;
    }
    elsif ($command =~ /^b (.+)$/) # set breakpoints from proto
    {
        _process_new_breakpoints( $1 );
    }
    elsif ($command =~ /^p (.+)$/) # Run to cursor
    {
        _set_run_to_cursor_breakpoint( $1 );
        _release_the_hounds();
        return;
    }
    elsif ($command eq 'o') # over,
    {
        my $current_frame = _get_current_stack_frame;
        if (_is_use_frame( $current_frame ))
        {
            $current_frame->{single} = STEP_INTO;
            $DB::single = STEP_CONTINUE;
        }
        else
        {
            $DB::single = STEP_OVER;
        }
        return;
    }
    elsif ($command =~ /^getchildren (.+)$/) # expand,
    {
        _get_reference_subelements( $1 );
    }
    elsif ($command =~ /^get_source (.+)$/) # get eval/file source
    {
        _get_file_source_handler( $1 );
    }
    elsif ($command eq 'u') # step out
    {
        my $current_frame = _get_current_stack_frame;
        if (_is_use_frame( $current_frame ))
        {
            $current_frame->{single} = STEP_CONTINUE;
        }
        $DB::single = STEP_CONTINUE;

        return;
    }
    else
    {
        $DB::single = STEP_INTO;
        return;
    }
    return 1;
}

my $input_buffer = '';

sub _get_next_command
{
    my $read_bytes;
    my $new_line_index = index $input_buffer, "\n";

    if ($new_line_index == -1)
    {
        while( $read_bytes = sysread( $_debug_socket, $input_buffer, 10240, length( $input_buffer ) ))
        {
            last if ($new_line_index = index $input_buffer, "\n") > -1;
        }
        unless (defined $read_bytes)
        {
            die 'Debugging socket disconnected';
            print STDERR "$!\n";
            exit -1;
        }
        unless ($new_line_index > -1)
        {
            print STDERR "Buffer $input_buffer has no newlines in it and nothing is in the socket\n";
            exit -1;
        }
    }
    my $command = substr $input_buffer, 0, $new_line_index + 1, '';
    $command =~ s/[\r\n]+$//;
    #    printf STDERR "Got command: %s and left in buffer %s\n%s\n\%s\n", length $command, length $input_buffer ,$command, $input_buffer;
    return $command;
}

sub _can_read
{
    return length( $input_buffer ) > 0 || ($_debug_socket && $_debug_socket_select && scalar $_debug_socket_select->can_read( 0 ));
}

sub _event_handler
{
    my ($breakpoint) = @_;
    _send_loaded_files_names();

    if ($breakpoint && !$breakpoint->{run_to_cursor})
    {
        _send_breakpoint_reached_event( $breakpoint );
    }
    else
    {
        _send_event( "STOP", _calc_stack_frames() );
    }

    while()
    {
        _report "Waiting for input\n" if $_dev_mode;
        _process_command( _get_next_command ) || return;
    }
}


sub _enter_frame
{
    my ($old_db_single, $wantarray) = @_;

    _report "Entering frame %s: %s %s-%s-%s, %s",
        scalar @$_stack_frames + 1,
        $DB::sub,
        $DB::trace // 'undef',
        $DB::signal // 'undef',
        $old_db_single // 'undef',
            $wantarray ? 'ARRAY' : defined $wantarray ? 'SCALAR' : 'VOID'
        if $_debug_sub_handler && $_dev_mode;

    $frame_prefix = $frame_prefix_step x (scalar @$_stack_frames + 1);

    my $new_stack_frame = {
        subname => $DB::sub,
        single  => $old_db_single,
    };
    push @{$_stack_frames}, $new_stack_frame;
    _set_break_points_for_files() if $_enable_compile_time_breakpoints && $ready_to_go;
    return $new_stack_frame;
}


sub _exit_frame
{
    $_internal_process = 1;
    my $frame = pop @$_stack_frames;
    $frame_prefix = $frame_prefix_step x (scalar @$_stack_frames);
    _report "Leaving frame %s, setting single to %s", (scalar @$_stack_frames + 1),
        $frame->{single} if $_debug_sub_handler && $_dev_mode;
    $DB::single = $frame->{single};
    $_internal_process = 0;
}

sub _get_normalized_perl_file_id
{
    my ($perl_file_id) = @_;
    if ($perl_file_id =~ /_<(.+)$/)
    {
        return $1;
    }
    else
    {
        die "PANIC: Incorrect perl file id $perl_file_id";
    }

}

sub _get_perl_file_id_by_real_path
{
    my ($path) = @_;

    return $path if $path =~ /^\(eval \d+\)/;
    return exists $_paths_to_perl_file_id_map{$path} ? $_paths_to_perl_file_id_map{$path} : undef;
}



sub _get_real_path_by_normalized_perl_file_id
{
    my $perl_file_id = shift;

    unless ($perl_file_id)
    {
        _dump_stack && _dump_frames && die "Perl normalized file id undefined";
    }

    if (!exists $_perl_file_id_to_path_map{$perl_file_id})
    {
        no strict 'refs';
        my $path = ${*{"::_<$perl_file_id"}};
        return '' unless defined $path; # some subs created via XS
        my $real_path = _calc_real_path( $path, $perl_file_id );

        $_perl_file_id_to_path_map{$perl_file_id} = $real_path;
        $_paths_to_perl_file_id_map{$real_path} = $perl_file_id;
    }
    return $_perl_file_id_to_path_map{$perl_file_id};
}

sub _get_real_path_by_perl_file_id
{
    my ($perl_file_id) = @_;
    return _get_real_path_by_normalized_perl_file_id( _get_normalized_perl_file_id( $perl_file_id ) );
}

sub _get_loaded_breakpoints_by_real_path
{
    my ($real_path) = @_;

    my $result = { };

    if ($_loaded_breakpoints{$real_path})
    {
        _report "Found real breakpoints" if $_dev_mode && $_debug_breakpoints;
        %$result = %{$_loaded_breakpoints{$real_path}};
    }

    # append breakpoints from templates
    if (my $substituted_file_descriptor = $_evals_to_templates_map{$real_path})
    {
        my ($template_path, $lines_map) = @$substituted_file_descriptor{qw/path lines_map/};
        _report "Found template file %s", $template_path if $_dev_mode && $_debug_breakpoints;
        if (my $template_breakpoints = $_loaded_breakpoints{$template_path})
        {
            _report "Found template breakpoints" if $_dev_mode && $_debug_breakpoints;
            foreach my $line (keys %$template_breakpoints)
            {
                if (my $mapped_line = $lines_map->{$line})
                {
                    _report "Got mapped breakpoint %s => %s", $line, $mapped_line if $_dev_mode && $_debug_breakpoints;
                    $result->{$mapped_line} //= $template_breakpoints->{$line};
                }
            }
        }
    }

    return scalar keys %$result ? $result : undef;
}

sub _get_current_breakpoint
{
    return if $DB::single || $DB::signal;
    my $current_breakpoint = $DB::dbline{$current_line};
    return unless $current_breakpoint;
    if ($current_breakpoint->{run_to_cursor})
    {
        $current_breakpoint->{remove} = 1;
        $current_breakpoint->{line}--; # fixme find out why works without it
        _process_breakpoints_descriptors( [ $current_breakpoint ] );
    }
    return $current_breakpoint;
}

sub _eval_expression
{
    my ($expression ) = @_;

    my $expr = "no strict; package $current_package;".'( $@, $!, $^E, $,, $/, $\, $^W ) = @DB::saved;'."$expression";
    _report "Running %s\n", $expr if $_dev_mode;

    my $result;

    {
        local $SIG{__WARN__} = sub {};
        $result = eval $expr;
    }

    if (my $e = $@)
    {
        # fixme handle object exceptions
        unless (ref $e) # message, change it
        {
            $e = join "; ", map {s/ at \(eval \d+.+$//;
                    $_ } grep $_, split /[\r\n]+/, $e;
        }
        $result = {
            error  => \1,
            result => $e
        };
    }
    else
    {
        $result = {
            error  => \0,
            result => $result
        };
    }

    return $result;
}

sub _reset_breakpoint
{
    my ($breakpoint_descriptor, $real_line, $perl_breakpoints_map) = @_;

    my $real_path = $breakpoint_descriptor->{path};

    if (exists $_loaded_breakpoints{$real_path} && exists $_loaded_breakpoints{$real_path}->{$real_line})
    {
        delete $_loaded_breakpoints{$real_path}->{$real_line};
    }

    if ($perl_breakpoints_map)
    {
        $perl_breakpoints_map->{$real_line} = 0;
    }

    return 1;
}

sub _set_run_to_cursor_breakpoint
{
    my ($serialized_descriptor) = @_;
    my $descriptor = _deserialize( $serialized_descriptor );
    @$descriptor{qw/run_to_cursor condition remove suspend/} = (1, undef, undef, \1);
    _process_breakpoints_descriptors( [ $descriptor ] );
}

sub _set_breakpoint
{
    my ($breakpoint_descriptor, $real_line, $perl_breakpoints_map, $perl_source_lines) = @_;

    my $event_data = {
        path => $breakpoint_descriptor->{path},
        line => $breakpoint_descriptor->{line} - 1,
    };

    _report 'Setting breakpoint to %s, real line %s, %s', $breakpoint_descriptor->{path}, $real_line,
        $perl_source_lines->[$real_line] if $_dev_mode && $_debug_breakpoints;

    if (!defined $perl_source_lines->[$real_line] || $perl_source_lines->[$real_line] == 0)
    {
        _send_event( "BREAKPOINT_DENIED", $event_data );
        return 1;
    }
    else
    {
        $perl_breakpoints_map->{$real_line} = $breakpoint_descriptor;
        _send_event( "BREAKPOINT_SET", $event_data ) unless $breakpoint_descriptor->{run_to_cursor};
        return 1;
    }
}

sub _set_up_debugger
{
    my ($json_data) = @_;
    _report 'Setting up debugger: %s', $json_data if $_dev_mode;
    my $set_up_data = _deserialize( $json_data );
    $_script_charset = $set_up_data->{charset};
    _process_breakpoints_descriptors( $set_up_data->{breakpoints} );

    $_enable_compile_time_breakpoints = 1 if $set_up_data->{enableCompileTimeBreakpoints};
    $_enable_noninteractive_mode = 1 if $set_up_data->{enableNonInteractiveMode};

    my $start_mode = $set_up_data->{startMode};

    if ($set_up_data->{initCode})
    {
        eval $set_up_data->{initCode};
        die "*** Debugger init code error:\n$@" if $@;
    }

    if ($start_mode eq 'RUN')
    {
        return STEP_CONTINUE;
    }
    elsif ($start_mode eq 'COMPILE')
    {
        return STEP_INTO;
    }
    else # here we should have a RUN_TO_BREAKPOINT
    {
        $_skip_run_stop = 1;
        return STEP_CONTINUE;
    }
}

sub _set_up_after_connect
{
    my ($allow_fail) = @_;

    $_debug_socket->autoflush( 1 );
    $_debug_socket_select = IO::Select->new();
    $_debug_socket_select->add( $_debug_socket );

    _send_data_to_debugger( +{
        event   => 'READY',
        version => $VERSION,
    } );
    _report "Waiting for set up data..." if $_dev_mode;
    my $set_up_data = <$_debug_socket>;
    return if !defined $set_up_data && $allow_fail;
    die "Connection closed" unless defined $set_up_data;

    $ready_to_go = 1;
    $DB::single = _set_up_debugger( $set_up_data );
}

sub _process_new_breakpoints
{
    my ($json_data) = @_;
    _report "Processing breakpoints: %s", $json_data if $_dev_mode;
    return _process_breakpoints_descriptors( _deserialize( $json_data ) );
}

sub _process_breakpoints_descriptors
{
    my ($descriptors) = @_;

    foreach my $descriptor (@$descriptors)
    {
        $descriptor->{line}++;
        $descriptor->{condition} = _from_utf8( $descriptor->{condition} );
        $descriptor->{action} = _from_utf8( $descriptor->{action} );

        _report "Processing descriptor: %s %s %s", $descriptor->{path}, $descriptor->{line},
                $descriptor->{remove} ? 'remove' : 'set' if $_dev_mode;

        my ($real_path, $line) = @$descriptor{qw/path line/};
        $_loaded_breakpoints{$real_path} //= { };
        $_loaded_breakpoints{$real_path}->{$line} = $descriptor;
        $_queued_breakpoints_files{$real_path} = 1;
    }
    _set_break_points_for_files() if $ready_to_go;
}


sub _set_break_points_for_files
{
    return unless $ready_to_go;
    my ($paths_array) = @_;

    $paths_array = [ keys %_queued_breakpoints_files ] unless $paths_array;
    $paths_array = [ $paths_array ] unless ref $paths_array;
    return unless @{$paths_array};

    my $default_context = undef;

    foreach my $real_path (@{$paths_array})
    {
        _report "Setting breakpoints for %s", $real_path if $_dev_mode;

        # mapping real path to file id
        my $perl_file_id = $real_path =~ /^\(eval \d+\)/
            ? $real_path
            : exists $_paths_to_perl_file_id_map{$real_path} ? $_paths_to_perl_file_id_map{$real_path} : next;

        # getting perl source lines and breakpoints
        my $glob = $::{"_<$perl_file_id"};
        next unless $glob && *{$glob}{ARRAY} && scalar @{*{$glob}{ARRAY}};
        my $perl_source_lines = *{$glob}{ARRAY};
        my $perl_breakpoints_map = *{$glob}{HASH};

        # getting breakpoints passed from the IDE
        my $loaded_breakpoints_descriptors = _get_loaded_breakpoints_by_real_path( $real_path ) or next;

        # switching context
        my $old_context = _switch_context( $perl_file_id );
        $default_context //= $old_context;

        #    print STDERR "Attempting to set breakpoints for $real_path\n";
        my @lines = keys %{$loaded_breakpoints_descriptors};
        my $breakpoints_left = scalar @lines;

        foreach my $real_line (@lines)
        {
            my $breakpoint_descriptor = $loaded_breakpoints_descriptors->{$real_line};
            _report "Processing descriptor %s, %s, %s", @{$breakpoint_descriptor}{qw/path line remove/} if $_dev_mode;

            next if $real_line > $#$perl_source_lines; # compiled incompletely

            if ($breakpoint_descriptor->{remove})
            {
                $breakpoints_left -= _reset_breakpoint( $breakpoint_descriptor, $real_line, $perl_breakpoints_map );
            }
            else
            {
                $breakpoints_left -= _set_breakpoint( $breakpoint_descriptor, $real_line, $perl_breakpoints_map,
                    $perl_source_lines );
            }
        }

        delete $_queued_breakpoints_files{$real_path} unless $breakpoints_left;
    }
    _switch_context( $default_context );
}

#sub mydie
#{
#    my ($msg) = @_;
#    print "$msg\n";
#    print Carp::longmess;
#    foreach my $key (sort keys %::)
#    {
#        print $key."\n" if $key =~ /^_</;
#    }
#    exit -1;
#}

sub _calc_real_path
{
    my $path = shift;
    my $new_filename = shift;

    my $real_path;
    if ($path =~ /^\(eval (\d+)/)
    {
        $real_path = $path;
    }
    else
    {
        $real_path = eval {Cwd::realpath( $path )};
        unless ($real_path)
        {
            _report 'Unable to find real path for %s use as it is', $path if $_dev_mode;
            $real_path = $path;
        }
        $real_path =~ s{\\}{/}g;
    }

    _report "$new_filename real path is $real_path\n" if $trace_real_path && $_dev_mode;
    return $real_path;
}

sub _switch_context
{
    my ($context_key) = @_;
    return unless $context_key;
    $context_key =~ s/^_<//;
    no strict 'refs';
    my $current_context = $DB::dbline;
    *DB::dbline = *{"::_<$context_key"};
    return $current_context;
}


# When the execution of  your program reaches a point that can hold a breakpoint, the DB::DB() subroutine is called if
# any of the variables $DB::trace , $DB::single , or $DB::signal is true. These variables are not localizable. This
# feature is disabled when executing inside DB::DB() , including functions called from it unless $^D & (1<<30) is true.
sub step_handler
{
    return if $_internal_process || !$ready_to_go;
    $_internal_process = 1;

    # Save eval failure, command failure, extended OS error, output field
    # separator, input record separator, output record separator and
    # the warning setting.
    @saved = ( $@, $!, $^E, $,, $/, $\, $^W );

    $, = "";      # output field separator is null string
    $/ = "\n";    # input record separator is newline
    $\ = "";      # output record separator is null string
    $^W = 0;      # warnings are off

    # set breakpoints for evals if any appeared
    _set_break_points_for_files() if $ready_to_go;

    # updating current position
    my @caller = caller();
    ($current_package, $current_file_id, $current_line) = @caller[0, 1, 2];

    if (defined $current_file_id)
    {
        _report( <<'EOM',
Calling %s %s
EOM
            _format_caller( @caller ),
            ${^GLOBAL_PHASE} // 'unknown',
        ) if $_dev_mode;
        _switch_context( $current_file_id );
    }
    else
    {
        _dump_stack && _dump_frames && warn "CAN'T FIND CALLER;\n";
    }

    my $skip_event_handler = 0;
    my $breakpoint;
    if ($breakpoint = _get_current_breakpoint)
    {
        my $condition = $breakpoint->{condition};

        if ($condition && !_eval_expression( $condition )->{result})
        {
            ( $@, $!, $^E, $,, $/, $\, $^W ) = @saved;
            $_internal_process = 0;
            return;
        }

        if (my $action = $breakpoint->{action})
        {
            $breakpoint->{action_result} = _to_utf8( _eval_expression( $action )->{result} );
        }

        if (!$breakpoint->{suspend})
        {
            _send_breakpoint_reached_event( $breakpoint );
            ( $@, $!, $^E, $,, $/, $\, $^W ) = @saved;
            $_internal_process = 0;
            return;
        }

        foreach my $frame (@{$_stack_frames})
        {
            $frame->{single} = STEP_INTO;
        }
        $DB::single = STEP_INTO;
    }
    elsif ($DB::single && $_skip_run_stop)
    {
        $_skip_run_stop = 0;
        $skip_event_handler = 1;
        $DB::single = STEP_CONTINUE;
    }
    elsif ($_skip_run_stop)
    {
        $_skip_run_stop = 0;
    }

    my $old_db_single = $DB::single;
    $DB::single = STEP_CONTINUE;

    _report "Step with %s %s %s, %s-%s-%s %s",
        $current_package // 'undef',
        $current_file_id // 'undef',
        $current_line // 'undef',
        $DB::trace // 'undef',
        $DB::signal // 'undef',
        $old_db_single // 'undef',
        ${^GLOBAL_PHASE}
        if $_dev_mode;

    _report $DB::dbline[$current_line] if $_dev_mode;
    _event_handler( $breakpoint ) unless $skip_event_handler;

    $_internal_process = 0;
    ( $@, $!, $^E, $,, $/, $\, $^W ) = @saved;
    ();
}

#
# This is a hook for templating engines working using perl evals.
# This hook should be invoked after evaluation of compiled template with template path
# and map of lines template_line => compiled_source_line
#
#    {
#        no strict 'refs';
#        my $glob = *{'::DB::template_handler'};
#
#        if ($glob && *{$glob}{CODE})
#        {
#            *{$glob}{CODE}->($filepath, $lines_map);
#        }
#    }
#
#
sub template_handler
{
    my ($real_path, $lines_map) = @_;

    my $last_eval_id = 0;
    my $eval_target;
    foreach my $main_key (keys %::)
    {
        if ($main_key =~ /^_<(\(eval (\d+)\).+?)$/)
        {
            if ($last_eval_id < $2)
            {
                $last_eval_id = $2;
                $eval_target = $1;
            }
        }
    }

    if ($last_eval_id)
    {
        $real_path = Cwd::realpath( $real_path );

        $_evals_to_templates_map{$eval_target} = {
            path      => $real_path,
            lines_map => $lines_map
        };
        $_templates_to_evals_map{$real_path} //= {
            lines_map => $lines_map,
            evals     => [ ]
        };
        push @{$_templates_to_evals_map{$real_path}->{evals}}, $eval_target;

        delete $_file_name_sent{$eval_target}; # forces re-sending file descriptor
        _report "Mapped template: %s to eval %s", $real_path, $eval_target if $_dev_mode;

        $_queued_breakpoints_files{$eval_target} = 1;
        _set_break_points_for_files() if $ready_to_go;
    }
    else
    {
        _report "Unable to locate top level eval for %s", $real_path if $_dev_mode;
    }
}


# this pass-through flag handles quotation overload loop
sub sub_handler
{
    my $stack_frame = undef;

    my $old_db_single = $DB::single;
    my $wantarray = wantarray;

    if (!$_internal_process)
    {
        $_internal_process = 1;

        _process_command( _get_next_command ) while $_enable_noninteractive_mode && _can_read;
        $old_db_single = $DB::single; # might be overriden in commands

        $DB::single = STEP_CONTINUE;

        $stack_frame = _enter_frame( $old_db_single, $wantarray );

        if ($current_package && $current_package eq 'DB')
        {
            _report "PANIC: Catched internal call" if $_dev_mode;
            _dump_stack && _dump_frames();
            die;
        }

        $DB::single = $old_db_single;
        $_internal_process = 0;
    }

    my $stack_pointer = $#$_stack_frames;

    if ($DB::single == STEP_OVER)
    {
        _report "Disabling step in in subcalls, will restore %s\n",
            $_stack_frames->[-1]->{single}
            if $_debug_sub_handler && $_dev_mode;
        $DB::single = STEP_CONTINUE;

        #        my $die_handler = $SIG{__DIE__};
        #        local $SIG{__DIE__} = sub{
        #            _hold_the_line;
        #            goto &$die_handler if $die_handler;
        #        };
    }
    else
    {
        _report "Keeping step as %s\n", $old_db_single if $stack_frame && $_debug_sub_handler && $_dev_mode;
    }

    if ($DB::sub eq 'DESTROY' or substr( $DB::sub, -9 ) eq '::DESTROY' or !defined $wantarray)
    {
        no strict 'refs';
        &$DB::sub;
        $#$_stack_frames = $stack_pointer;
        if ($stack_frame)
        {
            _exit_frame();
        }
        else
        {
            $DB::single = $old_db_single;
        }

        $DB::ret = undef; # return value
    }
    elsif ($wantarray)
    {
        no strict 'refs';
        my @result = &$DB::sub;
        $#$_stack_frames = $stack_pointer;
        if ($stack_frame)
        {
            _exit_frame();
        }
        else
        {
            $DB::single = $old_db_single;
        }
        @DB::ret = @result; # return value
    }
    else
    {
        no strict 'refs';
        my $result = &$DB::sub;
        $#$_stack_frames = $stack_pointer;
        if ($stack_frame)
        {
            _exit_frame();
        }
        else
        {
            $DB::single = $old_db_single;
        }
        $DB::ret = $result; # return value
    }
}

# If the call is to an lvalue subroutine, and &DB::lsub is defined &DB::lsub (args) is called instead, otherwise
# falling back to &DB::sub (args).
#sub lsub_handler: lvalue
#{
#    my $stack_frame = undef;
#
#    my $old_db_single = $DB::single;
#    if (!$_internal_process)
#    {
#        $_internal_process = 1;
#
#        $DB::single = STEP_CONTINUE;
#        $stack_frame = _enter_frame( $old_db_single );
#
#        $DB::single = $old_db_single;
#        $_internal_process = 0;
#    }
#
#    if ($DB::single == STEP_OVER)
#    {
#        _report "Disabling step in in subcalls\n" if $_dev_mode;
#        $DB::single = STEP_CONTINUE;
#    }
#    else
#    {
#        _report "Keeping step as %s\n", $old_db_single if $stack_frame if $_dev_mode;
#    }
#
#    {
#        no strict 'refs';
#        my $result = &$DB::sub;
#        if ($stack_frame)
#        {
#            _exit_frame();
#        }
#        else
#        {
#            $DB::single = $old_db_single;
#        }
#        return $DB::ret = $result;
#    }
#}


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

    my $old_internal_process = $_internal_process;
    $_internal_process = 1;

    my $perl_file_id = $_[0];
    my $real_path = _get_real_path_by_perl_file_id( $perl_file_id );

    _report "Loading module: %s => %s %s-%s-%s",
        $perl_file_id,
        $real_path,
        $DB::trace // 'undef',
        $DB::signal // 'undef',
        $old_db_single // 'undef',
        if $_debug_load_handler && $_dev_mode
    ;

    _set_break_points_for_files() if $ready_to_go;

    $_internal_process = $old_internal_process;

    $DB::single = $old_db_single;
}
# When execution of the program uses goto to enter a non-XS subroutine and the 0x80 bit is set in $^P , a call to
# &DB::goto is made, with $DB::sub holding the name of the subroutine being entered.
#$^P |= FLAG_REPORT_GOTO;
#sub goto_handler
#{
#    return if $_internal_process;
#    $_internal_process = 1;
#
#    my $old_db_single = $DB::single;
#    $DB::single = STEP_CONTINUE;
#
#    _report "Goto called%s from %s-%s-%s-%s",
#            scalar @_ ? ' with '.(join ',', @_) : '',
#        $DB::trace // 'undef',
#        $DB::signal // 'undef',
#        $old_db_single // 'undef',
#        ${^GLOBAL_PHASE} // 'unknown',
#     if $_dev_mode;
#    $DB::single = $old_db_single;
#    $_internal_process = 0;
#}

unless ($ENV{PERL5_DEBUG_ROLE} && $ENV{PERL5_DEBUG_HOST} && $ENV{PERL5_DEBUG_PORT})
{
    printf STDERR <<'EOM', map $_ // 'undefined', @ENV{qw/PERL5_DEBUG_ROLE PERL5_DEBUG_HOST PERL5_DEBUG_PORT/};
Can't start debugging session. In order to make it work, you should set up environment variables:

PERL5_DEBUG_ROLE - set this to 'server' if you want to make Perl process act as a server, and to 'client' to make it connect to IDEA.
PERL5_DEBUG_HOST - host to bind or connect, depending on role.
PERL5_DEBUG_PORT - host to listen or connect, depending on role.

Atm we've got:
 PERL5_DEBUG_ROLE=%s
 PERL5_DEBUG_HOST=%s
 PERL5_DEBUG_PORT=%s
EOM

    exit;
}

my $_connect_at_start = exists $ENV{PERL5_DEBUG_AUTOSTART} ? $ENV{PERL5_DEBUG_AUTOSTART} : 1;

sub is_connected
{
    return !!$_debug_socket;
}

sub _connect
{
    my ($attempts, $allow_fail) = @_;
    # http://perldoc.perl.org/perlipc.html#Sockets%3a-Client%2fServer-Communication
    if ($ENV{PERL5_DEBUG_ROLE} eq 'server')
    {
        printf STDERR "Listening for the IDE connection at %s:%s...\n", $ENV{PERL5_DEBUG_HOST}, $ENV{PERL5_DEBUG_PORT};
        my $_server_socket = IO::Socket::INET->new(
            Listen    => 1,
            LocalAddr => $ENV{PERL5_DEBUG_HOST},
            LocalPort => $ENV{PERL5_DEBUG_PORT},
            ReuseAddr => 1,
            Proto     => 'tcp',
        ) || die "Error binding to $ENV{PERL5_DEBUG_HOST}:$ENV{PERL5_DEBUG_PORT}";
        $_debug_packed_address = accept( $_debug_socket, $_server_socket );
    }
    else
    {
        foreach my $attempt (1 .. $attempts)
        {
            printf STDERR "($attempt)Connecting to the IDE from process %s at %s:%s...\n", $$, $ENV{PERL5_DEBUG_HOST},
                $ENV{PERL5_DEBUG_PORT};
            $_debug_socket = IO::Socket::INET->new(
                PeerAddr  => $ENV{PERL5_DEBUG_HOST},
                PeerPort  => $ENV{PERL5_DEBUG_PORT},
                ReuseAddr => 1,
                Proto     => 'tcp',
            );
            last if $_debug_socket || $attempt == $attempts;
            sleep( 1 ); # this is kinda hacky
        }
        die "Error connecting to $ENV{PERL5_DEBUG_HOST}:$ENV{PERL5_DEBUG_PORT}" if !$_debug_socket && !$allow_fail;
    }
    _set_up_after_connect( $allow_fail ) if $_debug_socket;
}

sub connect
{
    _connect( 1, 1 );
}

sub disconnect
{
    return unless is_connected();
    $_debug_socket->close();
    undef $_debug_socket_select;
    undef $_debug_socket;
    $ready_to_go = 0;
}

sub connect_or_reconnect
{
    disconnect() if is_connected();
    _connect( 1, 1 );
}

# we want disable() to completely bypass the debugger (except for the parts
# which are required for bookkeeping, like DB::postponed)
#
# setting %^P can disable DB::DB, but the only way to disable DB::sub is to
# make sure *DB::sub{CODE} is undef, while keeping %DB::sub and $DB::sub
# intact; the only way to do that is to save the glob slots we want to
# preserve, undef the glob and then restore the slots
#
# DB::lsub and DB::goto are easier, because we don't need to preserve the
# corresponding scalar/array/hash variables
my (%_orig_db_sub, %_disabled_db_sub, $_orig_db_lsub);

BEGIN
{
    %_orig_db_sub = %_disabled_db_sub = map {
        ($_ => *DB::sub{$_}) x !!*DB::sub{$_}
    } qw(SCALAR ARRAY HASH);
    $_orig_db_sub{CODE} = \&sub_handler;
    $_orig_db_lsub = undef; # \&lsub_handler
}

sub enable
{
    $^P = DEBUG_DEFAULT_FLAGS;
    undef *DB::sub;
    *DB::sub = $_orig_db_sub{$_} for keys %_orig_db_sub;
    #*DB::lsub = $_orig_db_lsub;
    #*DB::goto = \&goto_handler;
}

sub disable
{
    $DB::single = 0;
    $^P = DEBUG_PREPARE_FLAGS;
    undef *DB::sub;
    undef *DB::lsub;
    undef *DB::goto;
    *DB::sub = $_disabled_db_sub{$_} for keys %_disabled_db_sub;
}

push @$_stack_frames, {
        subname      => 'SCRIPT',
        file         => $current_file_id,
        current_line => $current_line,
        single       => STEP_INTO,
    };

_dump_stack && _dump_frames if $trace_code_stack_and_frames;

$_internal_process = 1;

require Cwd;
Cwd::getcwd();
require B::Deparse;
require JSON::XS;

$frame_prefix = $frame_prefix_step;

foreach my $main_key (keys %::)
{
    if ($main_key =~ /_<(.+)/)
    {
        _get_real_path_by_normalized_perl_file_id( $1 );
    }
}

*DB::DB = \&step_handler;
*DB::postponed = \&load_handler;

if ($_connect_at_start)
{
    _connect( 10, 0 );
    enable();
}

$_internal_process = 0;

1; # End of Devel::Camelcadedb
