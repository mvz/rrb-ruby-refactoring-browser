require 'rrb/node'

module RRB
  class GetTargetMethodVisitor < Visitor
    def initialize(method_name, namespace)
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
end
