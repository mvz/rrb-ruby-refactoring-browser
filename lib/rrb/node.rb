require 'forwardable'

module RRB

  class Visitor

    def initialize
      @error_message = ""
    end

    def visit_class( namespace, class_node )
    end

    def visit_method( namespace, method_node )
    end

    def visit_toplevel( namespace, top_node )
    end

    def visit_node( namespace, node )
    end

    def visit_singleton_method( namespace, node )
    end

    def visit_class_method( namespace, node )
    end

    def visit_singleton_class( namespace, node )
    end

    attr_reader :error_message
  end

  
  class Node    
    
    def initialize( name_id, scope, head_kw, tail_kw )
      @name_id = name_id
      @class_defs = scope.class_defs
      @method_defs = scope.method_defs
      @local_vars = scope.local_vars
      @global_vars = scope.global_vars
      @instance_vars = scope.instance_vars
      @class_vars = scope.class_vars
      @consts = scope.consts
      @method_calls = scope.method_calls
      @fcalls = scope.fcalls
      @singleton_method_defs = scope.singleton_method_defs
      @class_method_defs = scope.class_method_defs
      @singleton_class_defs = scope.singleton_class_defs
      @assigned = scope.assigned
      @attr_readers = scope.attr_readers
      @attr_writers = scope.attr_writers
      @attr_accessors = scope.attr_accessors
      @range = SyntaxRange.new( head_kw, tail_kw )
    end

    attr_reader :name_id, :class_defs, :method_defs, :method_calls, :local_vars
    attr_reader :global_vars, :instance_vars, :class_vars, :consts
    attr_reader :fcalls, :singleton_method_defs, :class_method_defs
    attr_reader :singleton_class_defs
    attr_reader :assigned
    attr_reader :attr_readers, :attr_writers, :attr_accessors
    attr_reader :range
    
    def calls
      @fcalls + @method_calls
    end

    def head_keyword
      @range.head
    end

    def tail_keyword
      @range.tail
    end
    
    def method_info( method_name )
      @method_defs.find{|m| m.name == method_name}
    end

    def class_info( class_name )
      @class_defs.find{|c| c.name == class_name }
    end
    
    def name
      @name_id.name
    end

    def accept( visitor, namespace )
      visitor.visit_node( namespace, self )
    end
    
    def accept_children( visitor, namespace )
      @class_defs.each{|i| i.accept( visitor, namespace ) }
      @method_defs.each{|i| i.accept( visitor, namespace ) }
      @singleton_method_defs.each{|i| i.accept( visitor, namespace ) }
      @class_method_defs.each{|i| i.accept( visitor, namespace ) }
      @singleton_class_defs.each{|i| i.accept( visitor, namespace ) }
    end
    
  end

  # represent one script file    
  class TopLevelNode < Node

    def initialize( scope )
      super IdInfo.new( :toplevel, nil, nil, 'toplevel' ), scope, nil, nil
    end
    
    def accept( visitor )
      visitor.visit_toplevel( NodeNamespace.new_toplevel, self )
      super visitor, NodeNamespace.new_toplevel
      accept_children( visitor, NodeNamespace.new_toplevel )
    end
    
  end

  # represent one module
  class ModuleNode < Node
    
    def accept( visitor, namespace )
      visitor.visit_class( namespace, self )
      super
      accept_children( visitor, NodeNamespace.new( self, namespace ) )
    end

  end

  # represent one class 
  class ClassNode < ModuleNode

    def initialize( name_id, scope, superclass, head_kw, tail_kw )
      super name_id, scope, head_kw, tail_kw
      @superclass = superclass      
    end

    attr_reader :superclass
  end
  
  # represent one method
  class MethodNode < Node

    def accept( visitor, namespace )
      visitor.visit_method( namespace, self )
      super
      accept_children( visitor, namespace )
    end
    
  end

  class SingletonMethodNode < Node

    def initialize( s_obj, method_name, scope, head_kw, tail_kw )
      @s_obj = s_obj
      super method_name, scope, head_kw, tail_kw
    end
    
    def accept( visitor, namespace )
      visitor.visit_singleton_method( namespace, self )
      super
      accept_children( visitor, namespace )
    end

    attr_reader :s_obj
  end

  class ClassMethodNode < Node

    def initialize( sdef )
      super sdef.name_id, sdef, sdef.head_keyword, sdef.tail_keyword
    end

    def accept( visitor, namespace )
      visitor.visit_class_method( namespace, self )
      super
      accept_children( visitor, namespace )
    end
    
  end

  class SingletonClassNode < Node

    def accept( visitor, namespace )
      visitor.visit_singleton_class( namespace, self )
      super
      accept_children( visitor, NodeNamespace.new( self, namespace ) )
    end
    
  end
  
  class IdInfo

    def initialize( type, lineno, pointer, name )
      @type = type
      @lineno = lineno
      @pointer = pointer
      @name = name
    end

    def adjust_id!( lineno, pointer )
      if @lineno > 1 then
	raise RRBError, "eval string mustn't have \"\\n\":#{self.inspect}"
      end
      @lineno = lineno
      @pointer += pointer
    end

    def head_pointer
      @pointer - @name.size
    end
    attr_reader :type, :lineno, :pointer, :name
    
  end

  class ConstInfo
    def initialize( toplevel, id, lconst=nil )
      if lconst == nil
	@elements_id = [ id ]
      else
	@elements_id = lconst.elements_id + [ id ]
      end
      @toplevel = toplevel      
    end
    
    def ConstInfo.new_toplevel( id )
      new( true, id )
    end
    
    def ConstInfo.new_colon2( id, lconst )
      new( lconst.toplevel?, id, lconst )
    end
    
    def ConstInfo.new_normal( id )
      new( false, id )
    end

    def basename
      @elements_id.map{|i| i.name}.join('::')
    end
    
    def name
      basename
    end

    def toplevel?
      @toplevel
    end

    def adjust_id!( lineno, pointer )
      @elements_id.last.adjust_id!( lineno, pointer )
    end

    def body
      @elements_id.last
    end
    
    attr_reader :elements_id
  end

  class SyntaxRange

    def initialize( head_kw, tail_kw )
      @head = head_kw
      @tail = tail_kw
    end

    attr_reader :head, :tail

    def contain?( range )
      @head.lineno < range.begin && range.end < @tail.lineno 
    end

    def out_of?( range )
      range.last < @head.lineno || @tail.lineno < range.first
    end
    
  end
  
  class NodeNamespace
    extend Forwardable

    def initialize( cur_node, cur_namespace )
      if cur_node.nil? then
	@nodes = []
      elsif cur_namespace.nil? then
	@nodes = [ cur_node ]
      else
	@nodes = cur_namespace.nodes + [ cur_node ]
      end
    end

    def NodeNamespace.new_toplevel
      new( nil, nil )
    end

    def name
      @nodes.map{|c| c.name}.join('::')
    end

    def match?( namespace )
      self.normal == namespace
    end

    def normal
      Namespace.new( @nodes.map{|c| c.name} )
    end

    def body_node
      @nodes.last
    end
    
    # this methods exist for test_node
    def_delegators :@nodes, :map, :last
    
    protected
    def nodes
      @nodes
    end
  end

  class Namespace
    include Enumerable

    # 3.4 -> 2.8
    class << self
      alias _new new
    end

    @@cache = Hash.new
    def Namespace.new( ns )
      if @@cache.has_key?( ns ) then
        @@cache[ns]
      else
        _new( ns )
      end
    end

    def initialize( ns )
      case ns
      when Array
	@namespace = ns
      when String
        @@cache[ns] = self
	@namespace = ns.split('::')
        @namespace.shift if @namespace[0] == ""
      else
	raise TypeError, 'must be string or array'
      end
      @namespace.freeze
    end
    
    def Namespace.[]( arg )
      new( arg )
    end
    
    def name
      @namespace.join('::')
    end

    def ary
      @namespace
    end

    def ==(other)
      return false unless other.kind_of?( Namespace )
      ary == other.ary
    end

    def eql?(other)
      self == other
    end

    def contain?( other )
      @namespace == other.ary[0,@namespace.size]
    end
    
    def hash
      ary.hash
    end

    def inspect
      "#<RRB::NS: #{name}>"
    end

    def chop
      return nil if @namespace.empty?
      Namespace.new( @namespace[0..-2] )
    end

    def <=>(other)
      self.ary <=> other.ary
    end
    
    Toplevel = Namespace.new( [] )
    Object = Namespace.new( ["Object"] )
  end

  # shortcut name
  NS = Namespace
  class Method

    def initialize( namespace, method_node )
      @namespace = namespace
      @node = method_node
    end
    
    def bare_name
      if @node
        @node.name
      else
        ""
      end
    end
    
    def local_vars
      Set.new( @node.local_vars.map{|var| var.name} )
    end

    def name
      if @namespace
        @namespace.name + '#' + bare_name
      else
        bare_name
      end
    end

    def self.new_toplevel
      new( nil, nil )
    end
    
  end
  
end
