require 'rrb/script'
require 'set'

module RRB

  class RefactableMethodsVistor < Visitor

    class Method

      def initialize( namespace, method_node )
	@namespace = namespace
	@node = method_node
      end

      def fullname
	@namespace.str + '#' + @node.name
      end

      def local_vars
	Set.new( @node.local_vars.map{|var| var.name} )
      end

      def name
	@node.name
      end
      
    end

    
    def initialize
      @methods = []
    end
    
    def visit_method( namespace, method_node )
      @methods.push RefactableMethodsVistor::Method.new( namespace, method_node )
    end

    attr_reader :methods
  end

  class RefactableClassesIVarsVisitor < Visitor

    def initialize
      @classes = Hash.new
    end

    def visit_method( namespace, node )
      @classes[namespace.str] ||= Set.new
      @classes[namespace.str].merge( node.instance_vars.map{|ivar| ivar.name} )
    end

    attr_reader :classes
  end
  
  class ScriptFile

    def refactable_methods
      visitor = RefactableMethodsVistor.new
      @tree.accept( visitor )
      visitor.methods
    end

    def refactable_classes_instance_vars
      visitor = RefactableClassesIVarsVisitor.new
      @tree.accept( visitor )
      visitor.classes
    end
    
  end
  
  class Script

    def refactable_methods
      @files.inject([]) do |ary, scriptfile|
	ary + scriptfile.refactable_methods
      end
    end

    def refactable_classes_instance_vars
      result = Hash.new
      @files.each do |scriptfile|
	scriptfile.refactable_classes_instance_vars.each do |name,ivars|
	  result[name] ||= Set.new
	  result[name].merge( ivars )
	end
      end

      result
    end
    
  end
end
