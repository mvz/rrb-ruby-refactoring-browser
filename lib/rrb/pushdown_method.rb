require 'rrb/scriptfile'
require 'rrb/script'
require 'rrb/node.rb'
require 'rrb/parser.rb'
require 'rrb/common_visitor.rb'

module RRB
  
  class PushdownMethodVisitor < Visitor

    def initialize(old_namespace, method_name, new_namespace)
      @method_name = method_name
      @old_namespace = old_namespace
      @new_namespace = new_namespace
      @superclass_range = nil
      @subclass_lineno = nil
    end

    attr_reader :superclass_range, :subclass_lineno

    def visit_class(namespace, node)
      cur_namespace = NodeNamespace.new(node, namespace)
      if cur_namespace.match?(@new_namespace) 
        @subclass_lineno = node.range.head.lineno
      elsif cur_namespace.match?(@old_namespace)
        node.method_defs.each do |method|
          if method.name == @method_name 
            @superclass_range = method.range
          end
        end
      end      
    end
  end

  class PushdownMethodCheckVisitor < Visitor
    def initialize(dumped_info, old_namespace, method_name, new_namespace)
      @dumped_info = dumped_info
      @method_name = method_name
      @old_namespace = old_namespace
      @new_namespace = new_namespace
      @result = true
    end

    attr_reader :result

    def visit_method(namespace, node)
      if namespace.match?(@old_namespace)
        if node.name == @method_name
          node.calls.each do |call|
            if @dumped_info[@old_namespace].private_method_names.include?(call.name)
              @result = false
            end
            if @dumped_info[@new_namespace].has_method?(call.name, false)
              @result = false
            end
          end
        else
          node.calls.each do |call|
            if call.name == @method_name
              @result = false
            end
          end
        end
      end
    end
  end

    
  class ScriptFile

    def pushdown_method(old_namespace, method_name, new_namespace, pushdowned_method)
      visitor = MoveMethodVisitor.new(old_namespace, method_name, new_namespace)
      @tree.accept( visitor )
      @new_script = RRB.insert_str(@input, visitor.insert_lineno, visitor.delete_range, pushdowned_method)
    end

    def pushdown_method?(dumped_info, old_namespace, method_name, new_namespace)
      visitor = PushdownMethodCheckVisitor.new(dumped_info, old_namespace, method_name, new_namespace)
      @tree.accept(visitor)
      return visitor.result
    end
  end

  class Script
    def pushdown_method(old_namespace, method_name, new_namespace)
      pushdowned_method = get_string_of_method(old_namespace, method_name)
      @files.each do |scriptfile|
	scriptfile.pushdown_method(old_namespace, method_name, new_namespace, pushdowned_method)
      end      
    end

    def pushdown_method?(old_namespace, method_name, new_namespace)
      return false unless get_dumped_info[old_namespace].has_method?(method_name, false)
      return false if get_dumped_info[new_namespace].has_method?(method_name, false)
      @files.each do |scriptfile|
        unless scriptfile.pushdown_method?(get_dumped_info, old_namespace, method_name, new_namespace)
          return false
        end
      end
      return true
    end
  end
end
