# This file is generated by Dist::Zilla
# Prereqs are detected automatically. You do not need to edit this file

requires "B::Deparse" => "0";
requires "Cwd" => "0";
requires "Encode" => "0";
requires "Hash::StoredIterator" => "0";
requires "IO::Socket::INET" => "0";
requires "JSON::XS" => "3.02";
requires "PadWalker" => "2.2";
requires "Scalar::Util" => "0";

on 'test' => sub {
  requires "Test::More" => "0";
};

on 'configure' => sub {
  requires "ExtUtils::MakeMaker" => "0";
};
