require 'rrb/node'

module RRB

  class MoveMethodVisitor < Visitor

    def initialize(old_namespace, method_name, new_namespace,
                   ignore_new_namespace, specified_lineno)
      @method_name = method_name
      @old_namespace = old_namespace
      @new_namespace = new_namespace
      @delete_range = nil
      @insert_lineno = nil
      @ignore_new_namespace = ignore_new_namespace
      @specified_lineno = specified_lineno
      @insert_lineno_decided = false
    end

    attr_reader :delete_range, :insert_lineno

    def visit_class(namespace, node)
      cur_namespace = NodeNamespace.new(node, namespace)
      if cur_namespace.match?(@new_namespace) && !@ignore_new_namespace
        unless @insert_lineno_decided
          @insert_lineno = node.range.head.lineno
          if node.range.head.lineno <= @specified_lineno && @specified_lineno <= node.range.tail.lineno
            @insert_lineno_decided = true
          end
        end
      elsif cur_namespace.match?(@old_namespace)
        if @method_name.instance_method?
          target_method = node.method_defs.find(){|method| method.name == @method_name.name}
          @delete_range = target_method && target_method.range
        end
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

    def get_string_of_method(namespace, node)
      if namespace.match?(@namespace)
        if node.name == @method_name.name
          @result_range = node.range
        end
      end
    end

    def visit_method(namespace, node)
      return unless @method_name.instance_method?
      get_string_of_method(namespace, node)
    end

    def visit_class_method(namespace, node)
      return unless @method_name.class_method?
      get_string_of_method(namespace, node)
    end
  end

  class GetClassOnRegionVisitor < Visitor
    def initialize( range)
      @range = range
      @namespace = NodeNamespace.new_toplevel
    end
    attr_reader :namespace

    def visit_class(namespace, node)
      if node.range.contain?( @range ) then
        @namespace = NodeNamespace.new(node, namespace)
      else
        unless node.range.out_of?(@range) then
          @namespace = nil
        end
      end
    end
  end
  class GetMethodOnRegionVisitor < Visitor
    def initialize( range)
      @range = range
      @method = Method.new_toplevel
    end
    attr_reader :method

    def visit_method(namespace, node)
      if node.range.contain?( @range ) then
        @method = Method.new(namespace, node)
      else
        unless node.range.out_of?( @range ) then
          @method = nil
        end
      end
    end
  end
  class CountNamespaceDefinitionVisitor < Visitor
    def initialize(namespace)
      @namespace = namespace
      @result = 0
    end

    attr_reader :result

    def visit_class(namespace, node)
      if NodeNamespace.new(node, namespace).match?(@namespace)
        @result += 1
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
      visitor = GetMethodOnRegionVisitor.new( range )
      @tree.accept( visitor )
      visitor.method
    end

    def get_class_on_region(range)
      visitor = GetClassOnRegionVisitor.new( range )
      @tree.accept( visitor )
      visitor.namespace
    end    
    
    def count_namespace_definition(namespace)
      visitor = CountNamespaceDefinitionVisitor.new(namespace)
      @tree.accept(visitor)
      visitor.result
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

    def get_class_on_cursor(path, lineno)
      get_class_on_region(path, lineno..lineno)
    end

    def get_method_on_cursor(path, lineno)
      get_method_on_region(path, lineno..lineno)
    end

    def count_namespace_definition(path, namespace)
      target_scriptfile = @files.find(){|scriptfile| scriptfile.path == path}
      target_scriptfile && target_scriptfile.count_namespace_definition(namespace)
    end

  end
end
