#
# $Id$
#
# Counts how many "if" are used in the Ruby script.
# Usage: count-if.rb <filename>
#

require 'ripper'

class KeywordCounter < Ripper
  def KeywordCounter.count(kw, src)
    c = new(src, kw)
    c.parse
    c.n
  end

  def initialize(src, kw)
    super src
    @kw = kw
    @n = 0
  end

  attr_reader :n

  def on__kw( word )
    @n += 1 if word == @kw
  end

  def method_missing( mid, *args )
    args[0]
  end
end

puts KeywordCounter.count('if', ARGF)
