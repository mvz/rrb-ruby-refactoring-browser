#!/usr/bin/env/ruby
#
# Usage: ruby lex.rb -e 'Object.new(1 + 2 * 3)'
#        ruby lex.rb filename
#

module RRB
end
require 'rrb_ripper.so'
require 'getopts'

def main
  ok = getopts(nil, 'e:', 'help')
  usage 0 if $OPT_help
  usage 1 unless ok
  if $OPT_e
    RubyLexer.lex($OPT_e).each {|pair| p pair }
  else
    RubyLexer.lex(ARGF).each {|pair| p pair }
  end
end

class RubyLexer < RRB::Ripper
  def RubyLexer.lex(src)
    r = new(src)
    r.parse
    r.tokens.map {|id, tok| [id.to_s.sub(/on__/,''), tok] }
  end

  def initialize(src)
    super src
    @tokens = []
  end

  attr_reader :tokens

  def method_missing(mid, *args)
    @tokens.push args if mid == :on__scan
  end
end

main
