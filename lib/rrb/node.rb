
module RRB

  class Visitor

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
    
  end

  
  class Node    
    
    def initialize( name_id, scope )
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
    end

    attr_reader :name_id, :class_defs, :method_defs, :method_calls, :local_vars
    attr_reader :global_vars, :instance_vars, :class_vars, :consts
    attr_reader :fcalls, :singleton_method_defs, :class_method_defs
    attr_reader :singleton_class_defs
    
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
      super IdInfo.new( :toplevel, nil, nil, 'toplevel' ), scope
    end
    
    def accept( visitor )
      visitor.visit_toplevel( [], self )
      super visitor, []
      accept_children( visitor, [] )
    end
    
  end

  # represent one module
  class ModuleNode < Node
    
    def accept( visitor, namespace )
      visitor.visit_class( namespace, self )
      super
      namespace.push self
      accept_children( visitor, namespace )
      namespace.pop 
    end

  end

  # represent one class 
  class ClassNode < ModuleNode
    
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

    def initialize( s_obj, method_name, scope )
      @s_obj = s_obj
      super method_name, scope
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
      @name_id = sdef.name_id
      @class_defs = sdef.class_defs
      @method_defs = sdef.method_defs
      @local_vars = sdef.local_vars
      @method_calls = sdef.method_calls
      @fcalls = sdef.fcalls
      @singleton_method_defs = sdef.singleton_method_defs
      @class_method_defs = sdef.class_method_defs
      @singleton_class_defs = sdef.singleton_class_defs
      @s_obj = sdef.s_obj
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
      namespace.push self
      accept_children( visitor, namespace )
      namespace.pop
    end
    
  end
  
  class IdInfo

    def initialize( type, lineno, pointer, name )
      @type = type
      @lineno = lineno
      @pointer = pointer
      @name = name
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
      if @toplevel then
	'::' + basename
      else
	basename
      end
    end

    def toplevel?
      @toplevel
    end
    attr_reader :elements_id
  end
  
end
