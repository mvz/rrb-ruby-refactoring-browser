require 'rrb/script'
require 'set'

module RRB

  class RefactableMethodsVistor < Visitor

    class Method

      def initialize( namespace, method_node )
	@namespace = namespace
	@node = method_node
      end

      def name
	@namespace.str + '#' + @node.name
      end

      def local_vars
	Set.new( @node.local_vars.map{|var| var.name} )
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
  
  class ScriptFile

    def refactable_methods
      visitor = RefactableMethodsVistor.new
      @tree.accept( visitor )
      visitor.methods
    end
    
  end
  
  class Script

    def refactable_methods
      @files.inject([]) do |ary, scriptfile|
	ary + scriptfile.refactable_methods
      end
    end

  end
end
