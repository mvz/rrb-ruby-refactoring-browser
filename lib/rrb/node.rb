
module RRB

  class Visitor

    def visit_class( namespace, class_node )
    end

    def visit_method( namespace, method_node )
    end
    
  end

  
  class Node    
    
    def initialize( name_id, scope )
      @name_id = name_id
      @class_defs = scope.class_defs
      @method_defs = scope.method_defs
      @local_vars = scope.local_vars
      @method_calls = scope.method_calls
    end

    attr_reader :name_id, :class_defs, :method_defs, :method_calls, :local_vars
    
    def method_info( method_name )
      @method_defs.find{|m| m.name == method_name}
    end

    def class_info( class_name )
      @class_defs.find{|c| c.name == class_name }
    end
    
    def name
      @name_id.name
    end

    def accept_children( visitor, namespace )
      @class_defs.each{|i| i.accept( visitor, namespace ) }
      @method_defs.each{|i| i.accept( visitor, namespace ) }
    end
    
  end

  # represent one script file    
  class TopLevelNode < Node

    def accept( visitor )
      accept_children( visitor, [] )
    end
    
  end

  # represent one module
  class ModuleNode < Node
    
    def accept( visitor, namespace )
      visitor.visit_class( namespace, self )
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
      accept_children( visitor, namespace )
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

end
