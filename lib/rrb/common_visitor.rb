require 'rrb/node'

module RRB

  class GetTargetMethodVisitor < Visitor
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
      @namespace = nil
    end
    attr_reader :namespace

    def visit_toplevel(namespace, node)
      @namespace = namespace
    end

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
      @namespace = nil
    end
    attr_reader :namespace

    def visit_toplevel(namespace, node)
      @namespace = namespace
    end

    def visit_method(namespace, node)
      if node.range.contain?( @start_lineno .. @end_lineno ) then
        @namespace = NodeNamespace.new(node, namespace)
      else
        unless node.range.out_of?(@start_lineno .. @end_lineno) then
          @namespace = nil
        end
      end
    end
  end
end
