require 'rrb/script'
require 'rrb/rename_constant'
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

  class RefactableClassesVisitor < Visitor

    def initialize
      @classes = Set.new
    end

    def visit_class( namespace, node )
      @classes << NodeNamespace.new(node, namespace).str
    end

    attr_reader :classes
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

  class RefactableClassesCVarsVisitor < Visitor
    def initialize
      @classes = Hash.new
    end

    def visit_class(namespace, node)
      class_name = NodeNamespace.new(node, namespace).str
      @classes[class_name] ||= Set.new
      @classes[class_name].merge(node.class_vars.map{|cvar| cvar.name})
    end

    attr_reader :classes
  end

  class RefactableGlobalVarsVisitor < Visitor

    def initialize
      @gvars = Set.new
    end
    
    def visit_node( namespace, node )
      @gvars.merge( node.global_vars.map{|gvar| gvar.name} )
    end

    attr_reader :gvars
  end
  
  class RefactableConstsVisitor < Visitor
    include ConstResolver
    
    def initialize(dumped_info)
      @dumped_info = dumped_info
      @consts = Set.new
    end

    def visit_node( namespace, node)
      ns = namespace.str
      if ModuleNode === node || SingletonClassNode === node
        ns << '::' unless namespace.str==""
        ns << node.name_id.name
      end

      node.consts.each do |constinfo|
        @consts << resolve_const(@dumped_info, ns, constinfo.name)
      end
    end

    def visit_class( namespace, node)
      @consts << resolve_const(@dumped_info, namespace.str, node.name_id.name)
    end

    attr_reader :consts
  end

  class ScriptFile

    def refactable_methods
      visitor = RefactableMethodsVistor.new
      @tree.accept( visitor )
      visitor.methods
    end

    def refactable_classes
      visitor = RefactableClassesVisitor.new
      @tree.accept( visitor )
      visitor.classes
    end

    def refactable_classes_instance_vars
      visitor = RefactableClassesIVarsVisitor.new
      @tree.accept( visitor )
      visitor.classes
    end

    def refactable_classes_class_vars
      visitor = RefactableClassesCVarsVisitor.new
      @tree.accept(visitor)
      visitor.classes
    end

    def refactable_global_vars
      visitor = RefactableGlobalVarsVisitor.new
      @tree.accept( visitor )
      visitor.gvars
    end
    
    def refactable_consts(dumped_info)
      visitor = RefactableConstsVisitor.new(dumped_info)
      @tree.accept( visitor )
      visitor.consts
    end
    
  end
  
  class Script

    def refactable_methods
      @files.inject([]) do |ary, scriptfile|
	ary + scriptfile.refactable_methods
      end
    end

    def refactable_classes
      result = Set.new
      @files.each do |scriptfile|
        result.merge scriptfile.refactable_classes
      end
      result
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

    def refactable_classes_class_vars
      result = Hash.new
      @files.each do |scriptfile|
        scriptfile.refactable_classes_class_vars.each do |name, cvars|
          result[name] ||= Set.new
          result[name].merge(cvars)
        end
      end
      result
    end

    def refactable_global_vars
      result = Set.new
      @files.each do |scriptfile|
        result.merge scriptfile.refactable_global_vars
      end
      result
    end
    
    def refactable_consts
      result = Set.new
      @files.each do |scriptfile|
        result.merge scriptfile.refactable_consts(get_dumped_info)
      end
      result
    end
  end
end
