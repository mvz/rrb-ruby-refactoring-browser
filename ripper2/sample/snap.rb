#
# extract method definitions from ruby script
#

require 'ripper'


class SnapshotFile

  def initialize( f )
    @f = f
    @ripper = nil
    @lines = []
  end

  attr_accessor :ripper

  def gets
    @lines.push @f.gets
    @lines.last
  end

  def mark( diff = 0 )
    Mark.new( @lines.size - 1, @ripper.pointer + diff )
  end

  def extract_from( m )
    extract m, mark()
  end

  def extract( m1, m2 )
    beg, fin = [m1, m2].sort

    tmp = []
    tmp.push @lines[beg.line][ beg.index .. -1 ]
    (beg.line + 1 ... fin.line).each do |i|
      tmp.push @lines[i]
    end
    tmp.push @lines[fin.line][ 0 ... fin.index ] if fin.line > beg.line

    tmp.join('')
  end


  class Mark

    include Comparable

    def initialize( n, i )
      @line = n
      @index = [i,0].max
    end

    attr_reader :line
    attr_reader :index

    def <=>( other )
      if @line > other.line then
        1
      elsif @line < other.line then
        -1
      else
        if @index > other.index then
          1
        elsif @index < other.index then
          -1
        else
          0
        end
      end
    end

  end

end


class MethodExtractor < Ripper

  def parse( f )
    @f = SnapshotFile.new(f)
    @f.ripper = self
    super @f
  end

  def on__KEYWORD( str )
    str == 'def' ? @f.mark(-str.size) : str
  end

  def on__def( tdef, *rest )
    puts correct_indent( @f.extract_from(tdef), tdef )
  end

  def correct_indent( str, mark )
    str[0,0] = ' ' * mark.index
    str
  end

end


MethodExtractor.parse ARGF
