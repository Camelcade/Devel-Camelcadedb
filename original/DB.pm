#
# Documentation is at the __END__
#

package DB;

# "private" globals

my ($running, $ready, $deep, $usrctxt, $evalarg, 
    @stack, @saved, @skippkg, @clients);
my $preeval = {};
my $posteval = {};
my $ineval = {};

####
#
# Globals - must be defined at startup so that clients can refer to 
# them right after a C<require DB;>
#
####

BEGIN {

  # these are hardcoded in perl source (some are magical)

  $DB::sub = '';        # name of current subroutine
  %DB::sub = ();        # "filename:fromline-toline" for every known sub
  $DB::single = 0;      # single-step flag (set it to 1 to enable stops in BEGIN/use)
  $DB::signal = 0;      # signal flag (will cause a stop at the next line)
  $DB::trace = 0;       # are we tracing through subroutine calls?
  @DB::args = ();       # arguments of current subroutine or @ARGV array
  @DB::dbline = ();     # list of lines in currently loaded file
  %DB::dbline = ();     # actions in current file (keyed by line number)
  @DB::ret = ();        # return value of last sub executed in list context
  $DB::ret = '';        # return value of last sub executed in scalar context

  # other "public" globals  

  $DB::package = '';    # current package space
  $DB::filename = '';   # current filename
  $DB::subname = '';    # currently executing sub (fully qualified name)
  $DB::lineno = '';     # current line number

  $DB::VERSION = $DB::VERSION = '1.08';

  # initialize private globals to avoid warnings

  $running = 1;         # are we running, or are we stopped?
  @stack = (0);
  @clients = ();
  $deep = 1000;
  $ready = 0;
  @saved = ();
  @skippkg = ();
  $usrctxt = '';
  $evalarg = '';
}

####
# entry point for all subroutine calls
#
sub sub {
  push(@stack, $DB::single);
  $DB::single &= 1;
  $DB::single |= 4 if $#stack == $deep;
  if ($DB::sub eq 'DESTROY' or substr($DB::sub, -9) eq '::DESTROY' or not defined wantarray) {
    &$DB::sub;
    $DB::single |= pop(@stack);
    $DB::ret = undef;
  }
  elsif (wantarray) {
    @DB::ret = &$DB::sub;
    $DB::single |= pop(@stack);
    @DB::ret;
  }
  else {
    $DB::ret = &$DB::sub;
    $DB::single |= pop(@stack);
    $DB::ret;
  }
}

####
# this is called by perl for every statement
#
sub DB {
  return unless $ready;
  &save;
  ($DB::package, $DB::filename, $DB::lineno) = caller;

  return if @skippkg and grep { $_ eq $DB::package } @skippkg;

  $usrctxt = "package $DB::package;";		# this won't let them modify, alas
  local(*DB::dbline) = "::_<$DB::filename";

  my ($stop, $action);
  if (($stop,$action) = split(/\0/,$DB::dbline{$DB::lineno})) {
    if ($stop eq '1') {
      $DB::signal |= 1;
    }
    else {
      $stop = 0 unless $stop;			# avoid un_init warning
      $evalarg = "\$DB::signal |= do { $stop; }"; &eval;
      $DB::dbline{$DB::lineno} =~ s/;9($|\0)/$1/;    # clear any temp breakpt
    }
  }
  if ($DB::single || $DB::trace || $DB::signal) {
    $DB::subname = ($DB::sub =~ /\'|::/) ? $DB::sub : "${DB::package}::$DB::sub"; #';
    DB->loadfile($DB::filename, $DB::lineno);
  }
  $evalarg = $action, &eval if $action;
  if ($DB::single || $DB::signal) {
    _outputall($#stack . " levels deep in subroutine calls.\n") if $DB::single & 4;
    $DB::single = 0;
    $DB::signal = 0;
    $running = 0;
    
    &eval if ($evalarg = DB->prestop);
    my $c;
    for $c (@clients) {
      # perform any client-specific prestop actions
      &eval if ($evalarg = $c->cprestop);
      
      # Now sit in an event loop until something sets $running
      do {
	$c->idle;                     # call client event loop; must not block
	if ($running == 2) {          # client wants something eval-ed
	  &eval if ($evalarg = $c->evalcode);
	  $running = 0;
	}
      } until $running;
      
      # perform any client-specific poststop actions
      &eval if ($evalarg = $c->cpoststop);
    }
    &eval if ($evalarg = DB->poststop);
  }
  ($@, $!, $,, $/, $\, $^W) = @saved;
  ();
}
  
####
# this takes its argument via $evalarg to preserve current @_
#    
sub eval {
  ($@, $!, $,, $/, $\, $^W) = @saved;
  eval "$usrctxt $evalarg; &DB::save";
  _outputall($@) if $@;
}

###############################################################################
#         no compile-time subroutine call allowed before this point           #
###############################################################################

use strict;                # this can run only after DB() and sub() are defined

sub save {
  @saved = ($@, $!, $,, $/, $\, $^W);
  $, = ""; $/ = "\n"; $\ = ""; $^W = 0;
}

sub catch {
  for (@clients) { $_->awaken; }
  $DB::signal = 1;
  $ready = 1;
}

####
#
# Client callable (read inheritable) methods defined after this point
#
####

sub register {
  my $s = shift;
  $s = _clientname($s) if ref($s);
  push @clients, $s;
}

sub done {
  my $s = shift;
  $s = _clientname($s) if ref($s);
  @clients = grep {$_ ne $s} @clients;
  $s->cleanup;
#  $running = 3 unless @clients;
  exit(0) unless @clients;
}

sub _clientname {
  my $name = shift;
  "$name" =~ /^(.+)=[A-Z]+\(.+\)$/;
  return $1;
}

sub next {
  my $s = shift;
  $DB::single = 2;
  $running = 1;
}

sub step {
  my $s = shift;
  $DB::single = 1;
  $running = 1;
}

sub cont {
  my $s = shift;
  my $i = shift;
  $s->set_tbreak($i) if $i;
  for ($i = 0; $i <= $#stack;) {
	$stack[$i++] &= ~1;
  }
  $DB::single = 0;
  $running = 1;
}

####
# XXX caller must experimentally determine $i (since it depends
# on how many client call frames are between this call and the DB call).
# Such is life.
#
sub ret {
  my $s = shift;
  my $i = shift;      # how many levels to get to DB sub
  $i = 0 unless defined $i;
  $stack[$#stack-$i] |= 1;
  $DB::single = 0;
  $running = 1;
}

####
# XXX caller must experimentally determine $start (since it depends
# on how many client call frames are between this call and the DB call).
# Such is life.
#
sub backtrace {
  my $self = shift;
  my $start = shift;
  my($p,$f,$l,$s,$h,$w,$e,$r,$a, @a, @ret,$i);
  $start = 1 unless $start;
  for ($i = $start; ($p,$f,$l,$s,$h,$w,$e,$r) = caller($i); $i++) {
    @a = @DB::args;
    for (@a) {
      s/'/\\'/g;
      s/([^\0]*)/'$1'/ unless /^-?[\d.]+$/;
      require 'meta_notation.pm';
      $_ = _meta_notation($_) if /[[:^print:]]/a;
    }
    $w = $w ? '@ = ' : '$ = ';
    $a = $h ? '(' . join(', ', @a) . ')' : '';
    $e =~ s/\n\s*\;\s*\Z// if $e;
    $e =~ s/[\\\']/\\$1/g if $e;
    if ($r) {
      $s = "require '$e'";
    } elsif (defined $r) {
      $s = "eval '$e'";
    } elsif ($s eq '(eval)') {
      $s = "eval {...}";
    }
    $f = "file '$f'" unless $f eq '-e';
    push @ret, "$w&$s$a from $f line $l";
    last if $DB::signal;
  }
  return @ret;
}

sub _outputall {
  my $c;
  for $c (@clients) {
    $c->output(@_);
  }
}

sub trace_toggle {
  my $s = shift;
  $DB::trace = !$DB::trace;
}


####
# without args: returns all defined subroutine names
# with subname args: returns a listref [file, start, end]
#
sub subs {
  my $s = shift;
  if (@_) {
    my(@ret) = ();
    while (@_) {
      my $name = shift;
      push @ret, [$DB::sub{$name} =~ /^(.*)\:(\d+)-(\d+)$/] 
	if exists $DB::sub{$name};
    }
    return @ret;
  }
  return keys %DB::sub;
}

####
# first argument is a filename whose subs will be returned
# if a filename is not supplied, all subs in the current
# filename are returned.
#
sub filesubs {
  my $s = shift;
  my $fname = shift;
  $fname = $DB::filename unless $fname;
  return grep { $DB::sub{$_} =~ /^$fname/ } keys %DB::sub;
}

####
# returns a list of all filenames that DB knows about
#
sub files {
  my $s = shift;
  my(@f) = grep(m|^_<|, keys %main::);
  return map { substr($_,2) } @f;
}

####
# returns reference to an array holding the lines in currently
# loaded file
#
sub lines {
  my $s = shift;
  return \@DB::dbline;
}

####
# loadfile($file, $line)
#
sub loadfile {
  my $s = shift;
  my($file, $line) = @_;
  if (!defined $main::{'_<' . $file}) {
    my $try;
    if (($try) = grep(m|^_<.*$file|, keys %main::)) {  
      $file = substr($try,2);
    }
  }
  if (defined($main::{'_<' . $file})) {
    my $c;
#    _outputall("Loading file $file..");
    *DB::dbline = "::_<$file";
    $DB::filename = $file;
    for $c (@clients) {
#      print "2 ", $file, '|', $line, "\n";
      $c->showfile($file, $line);
    }
    return $file;
  }
  return undef;
}

sub lineevents {
  my $s = shift;
  my $fname = shift;
  my(%ret) = ();
  my $i;
  $fname = $DB::filename unless $fname;
  local(*DB::dbline) = "::_<$fname";
  for ($i = 1; $i <= $#DB::dbline; $i++) {
    $ret{$i} = [$DB::dbline[$i], split(/\0/, $DB::dbline{$i})] 
      if defined $DB::dbline{$i};
  }
  return %ret;
}

sub set_break {
  my $s = shift;
  my $i = shift;
  my $cond = shift;
  $i ||= $DB::lineno;
  $cond ||= '1';
  $i = _find_subline($i) if ($i =~ /\D/);
  $s->output("Subroutine not found.\n") unless $i;
  if ($i) {
    if ($DB::dbline[$i] == 0) {
      $s->output("Line $i not breakable.\n");
    }
    else {
      $DB::dbline{$i} =~ s/^[^\0]*/$cond/;
    }
  }
}

sub set_tbreak {
  my $s = shift;
  my $i = shift;
  $i = _find_subline($i) if ($i =~ /\D/);
  $s->output("Subroutine not found.\n") unless $i;
  if ($i) {
    if ($DB::dbline[$i] == 0) {
      $s->output("Line $i not breakable.\n");
    }
    else {
      $DB::dbline{$i} =~ s/($|\0)/;9$1/; # add one-time-only b.p.
    }
  }
}

sub _find_subline {
  my $name = shift;
  $name =~ s/\'/::/;
  $name = "${DB::package}\:\:" . $name if $name !~ /::/;
  $name = "main" . $name if substr($name,0,2) eq "::";
  my($fname, $from, $to) = ($DB::sub{$name} =~ /^(.*):(\d+)-(\d+)$/);
  if ($from) {
    local *DB::dbline = "::_<$fname";
    ++$from while $DB::dbline[$from] == 0 && $from < $to;
    return $from;
  }
  return undef;
}

sub clr_breaks {
  my $s = shift;
  my $i;
  if (@_) {
    while (@_) {
      $i = shift;
      $i = _find_subline($i) if ($i =~ /\D/);
      $s->output("Subroutine not found.\n") unless $i;
      if (defined $DB::dbline{$i}) {
        $DB::dbline{$i} =~ s/^[^\0]+//;
        if ($DB::dbline{$i} =~ s/^\0?$//) {
          delete $DB::dbline{$i};
        }
      }
    }
  }
  else {
    for ($i = 1; $i <= $#DB::dbline ; $i++) {
      if (defined $DB::dbline{$i}) {
        $DB::dbline{$i} =~ s/^[^\0]+//;
        if ($DB::dbline{$i} =~ s/^\0?$//) {
          delete $DB::dbline{$i};
        }
      }
    }
  }
}

sub set_action {
  my $s = shift;
  my $i = shift;
  my $act = shift;
  $i = _find_subline($i) if ($i =~ /\D/);
  $s->output("Subroutine not found.\n") unless $i;
  if ($i) {
    if ($DB::dbline[$i] == 0) {
      $s->output("Line $i not actionable.\n");
    }
    else {
      $DB::dbline{$i} =~ s/\0[^\0]*//;
      $DB::dbline{$i} .= "\0" . $act;
    }
  }
}

sub clr_actions {
  my $s = shift;
  my $i;
  if (@_) {
    while (@_) {
      my $i = shift;
      $i = _find_subline($i) if ($i =~ /\D/);
      $s->output("Subroutine not found.\n") unless $i;
      if ($i && $DB::dbline[$i] != 0) {
	$DB::dbline{$i} =~ s/\0[^\0]*//;
	delete $DB::dbline{$i} if $DB::dbline{$i} =~ s/^\0?$//;
      }
    }
  }
  else {
    for ($i = 1; $i <= $#DB::dbline ; $i++) {
      if (defined $DB::dbline{$i}) {
	$DB::dbline{$i} =~ s/\0[^\0]*//;
	delete $DB::dbline{$i} if $DB::dbline{$i} =~ s/^\0?$//;
      }
    }
  }
}

sub prestop {
  my ($client, $val) = @_;
  return defined($val) ? $preeval->{$client} = $val : $preeval->{$client};
}

sub poststop {
  my ($client, $val) = @_;
  return defined($val) ? $posteval->{$client} = $val : $posteval->{$client};
}

#
# "pure virtual" methods
#

# client-specific pre/post-stop actions.
sub cprestop {}
sub cpoststop {}

# client complete startup
sub awaken {}

sub skippkg {
  my $s = shift;
  push @skippkg, @_ if @_;
}

sub evalcode {
  my ($client, $val) = @_;
  if (defined $val) {
    $running = 2;    # hand over to DB() to evaluate in its context
    $ineval->{$client} = $val;
  }
  return $ineval->{$client};
}

sub ready {
  my $s = shift;
  return $ready = 1;
}

# stubs
    
sub init {}
sub stop {}
sub idle {}
sub cleanup {}
sub output {}

#
# client init
#
for (@clients) { $_->init }

$SIG{'INT'} = \&DB::catch;

# disable this if stepping through END blocks is desired
# (looks scary and deconstructivist with Swat)
END { $ready = 0 }

1;
