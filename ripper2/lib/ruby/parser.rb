#
# ruby/parser.rb
#

require 'ripper'


module Ruby

  class Parser < Ripper

    def Parser.parse( str, fname = '(eval)', lineno = 1 )
      new(str, fname, lineno).parse
    end

    def Parser.parse_file( path )
      File.open(path) {|f|
        return new(f, path, 1).parse
      }
    end

    def on__arglist_new
      Args.new
    end

    def on__arglist_add( list, a )
      list.add a
    end

    def on__arglist_add_star( list, a )
      list.star = a
    end

    def on__arglist_add_block( list, a )
      list.block = a
    end

    def on__bare_assoc_hash( as )
      Hash.new(*as)
    end

  end


  class MLHS

    def initialize( init = [] )
      @trees = init
      @rest = nil
      @have_paren = false
    end

    def have_paren?
      @have_paren
    end

    def have_paren=( bool )
      @have_paren = bool
    end

    def add( mlhs )
      @trees.push mlhs
    end

    def rest=( field )
      @rest = field
    end

    def leaf?
      false
    end

    def to_s
      @trees.map {|t| t.to_s }.join(', ')
    end

  end


  class Field

    def leaf?
      true
    end

  end


  #
  # final field of mlhs  e.g.
  # a, b, c,      = mrhs
  #          ^^^ here
  #
  class NullField < Field

    def to_s
      ''
    end

  end


  class VariableField < Field

    def initialize( id )
      @ident = id
    end

    def to_s
      @ident
    end

  end


  class ObjectField < Field

    def initialize( recv, op, mid )
      @receiver = recv
      @op = op
      @method_id = mid
    end

    def to_s
      "#{@receiver}#{@op}#{@method_id}"
    end

  end


  class ConstField < Field

    def initialize( c )
      @ident = c
    end

    def to_s
      @ident
    end
  
  end


  class TopConstField < Field

    def initialize( c )
      @ident = c
    end

    def to_s
      "::#{@ident}"
    end
  
  end


  class ConstPathField < Field

    def initialize( lhs, c )
      @lhs = lhs
      @ident = c
    end

    def to_s
      "#{@lhs}::#{@ident}"
    end
  
  end


  class Args

    def initialize( list = [], star = nil, block = nil )
      @list = list
      @star = star
      @block = block
    end

    attr_reader :list
    attr_reader :star
    attr_reader :block

    def add( a )
      @list.push a
    end

    def prepend( a )
      @list.unshift a
    end

    def star=( a )
      raise ArgumentError, "`*' argument given twice" if @star
      @star = a
    end

    def block=( a )
      raise ArgumentError, "`&' argument given twice" if @block
      @block = a
    end

  end


  class Params
  
    def initialize( list = [], dflts = [], rest = nil, block = nil )
      @list = list
      @defaults = dflts
      @rest = rest
      @block = block
    end

    attr_reader :list
    attr_reader :defaults
    attr_reader :rest
    attr_reader :block

    def add( n )
      @list.push n
    end

    def prepend( n )
      @list.unshift n
    end

    def add_with_default( n, default )
      @defaults.push [n,defaults]
    end

    def default_value_of( n )
      a = @defaults.assoc(n) or return nil
      a[1]
    end

    def rest=( n )
      raise ArgumentError, "`*' parameter given twice" if @star
      @rest = n
    end

    def block=( n )
      raise ArgumentError, "`&' parameter given twice" if @block
      @block = n
    end
  
  end

end
