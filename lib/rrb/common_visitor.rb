require 'rrb/node'

module RRB

  class MoveMethodVisitor < Visitor

    def initialize(old_namespace, method_name, new_namespace)
      @method_name = method_name
      @old_namespace = old_namespace
      @new_namespace = new_namespace
      @delete_range = nil
      @insert_lineno = nil
    end

    attr_reader :delete_range, :insert_lineno

    def visit_class(namespace, node)
      cur_namespace = NodeNamespace.new(node, namespace)
      if cur_namespace.match?(@new_namespace) 
        @insert_lineno = node.range.head.lineno
      elsif cur_namespace.match?(@old_namespace)
        target_method = node.method_defs.find(){|method| method.name == @method_name}
        @delete_range = target_method && target_method.range
      end      
    end
  end

  class GetStringOfMethodVisitor < Visitor
    def initialize(namespace, method_name)
      @method_name = method_name
      @namespace = namespace
      @result_range = nil
    end

    attr_reader :result_range

    def visit_method(namespace, node)
      if namespace.match?(@namespace)
        if node.name == @method_name
          @result_range = node.range
        end
      end
    end
  end

  class GetClassOnRegionVisitor < Visitor
    def initialize(start_lineno, end_lineno)
      @start_lineno = start_lineno
      @end_lineno = end_lineno
      @namespace = NodeNamespace.new_toplevel
    end
    attr_reader :namespace

    def visit_class(namespace, node)
      if node.range.contain?( @start_lineno .. @end_lineno ) then
        @namespace = NodeNamespace.new(node, namespace)
      else
        unless node.range.out_of?(@start_lineno .. @end_lineno) then
          @namespace = nil
        end
      end
    end
  end
  class GetMethodOnRegionVisitor < Visitor
    def initialize(start_lineno, end_lineno)
      @start_lineno = start_lineno
      @end_lineno = end_lineno
      @method = Method.new_toplevel
    end
    attr_reader :method

    def visit_method(namespace, node)
      if node.range.contain?( @start_lineno .. @end_lineno ) then
        @method = Method.new(namespace, node)
      else
        unless node.range.out_of?(@start_lineno .. @end_lineno) then
          @method = nil
        end
      end
    end
  end

  class ScriptFile
    def get_string_of_method(namespace, method_name)
      visitor = GetStringOfMethodVisitor.new(namespace, method_name)
      @tree.accept(visitor)
      range = visitor.result_range
      range && @input.split(/^/)[range.head.lineno-1..range.tail.lineno-1].join
    end

    def get_method_on_region(range)
      visitor = GetMethodOnRegionVisitor.new(range.begin, range.end)
      @tree.accept( visitor )
      visitor.method
    end

    def get_class_on_region(range)
      visitor = GetClassOnRegionVisitor.new(range.begin, range.end)
      @tree.accept( visitor )
      visitor.namespace
    end    
    
  end

  class Script
    def get_string_of_method(namespace, method_name)
      @files.inject(nil) do |result, scriptfile|
        result ||= scriptfile.get_string_of_method(namespace, method_name)
      end
    end
    
    def get_class_on_region(path, range)
      target_scriptfile = @files.find(){|scriptfile| scriptfile.path == path}
      target_scriptfile && target_scriptfile.get_class_on_region(range)
    end
    
    def get_method_on_region(path, range)
      target_scriptfile = @files.find(){|scriptfile| scriptfile.path == path}
      target_scriptfile && target_scriptfile.get_method_on_region(range)
    end
  end
end
