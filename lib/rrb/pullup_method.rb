require 'rrb/scriptfile'
require 'rrb/script'
require 'rrb/node.rb'
require 'rrb/parser.rb'
require 'rrb/common_visitor'

module RRB

  class PullupMethodCheckVisitor < Visitor
    def initialize(dumped_info, old_namespace, method_name, new_namespace)
      @dumped_info = dumped_info
      @method_name = method_name
      @old_namespace = old_namespace
      @new_namespace = new_namespace
      @result = true
    end

    attr_reader :result

    def visit_method(namespace, node)
      if namespace.match?(@old_namespace) && node.name == @method_name
        subclass_info = @dumped_info[@old_namespace]
        node.calls.each do |call| 
          if subclass_info.has_method?(call.name, false)
            @result = false
          end
        end
      end
    end
  end

  class ScriptFile

    def pullup_method(old_namespace, method_name, new_namespace, pullupped_method)
      visitor = MoveMethodVisitor.new(old_namespace, method_name,  new_namespace)
      @tree.accept( visitor )
      @new_script = RRB.insert_str(@input, visitor.insert_lineno, visitor.delete_range, pullupped_method)
    end

    def pullup_method?(dumped_info, old_namespace, method_name, new_namespace)
      visitor = PullupMethodCheckVisitor.new(dumped_info, old_namespace, method_name, new_namespace)
      @tree.accept(visitor)
      return visitor.result
    end
  end

  class Script
    def pullup_method(old_namespace, method_name, new_namespace)
      pullupped_method = get_string_of_method(old_namespace, method_name)
      @files.each do |scriptfile|
	scriptfile.pullup_method(old_namespace, method_name, new_namespace, pullupped_method)
      end      
    end

    def pullup_method?(old_namespace, method_name, new_namespace)
      return false unless get_dumped_info[old_namespace].has_method?(method_name, false)
      return false if get_dumped_info[new_namespace].has_method?(method_name)

      @files.each do |scriptfile|
        unless scriptfile.pullup_method?(get_dumped_info, old_namespace, method_name, new_namespace)
          return false
        end
      end
      return true
    end
  end
end
