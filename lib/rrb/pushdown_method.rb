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
              @error_message = "#{@old_namespace.name}##{@method_name} calls private function \"#{call.name}\"\n"
            end
            if @dumped_info[@new_namespace].has_method?(call.name, false)
              @result = false
              @error_message = "Destination class also has #{call.name}\n"
            end
          end
        else
          node.calls.each do |call|
            if call.name == @method_name
              @result = false
              @error_message = "Other function uses #{@old_namespace.name}##{@method_name}\n"
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
      @error_message = visitor.error_message unless visitor.result 
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
      unless get_dumped_info[old_namespace].has_method?(method_name, false)
        @error_message = "#{old_namespace.name} doesn't have #{method_name}\n"
        return false
      end

      if get_dumped_info[new_namespace].has_method?(method_name, false)
        @error_message = "#{new_namespace.name} already has #{method_name}\n"
        return false
      end
      @files.each do |scriptfile|
        unless scriptfile.pushdown_method?(get_dumped_info, old_namespace, method_name, new_namespace)
          @error_message = scriptfile.error_message
          return false
          
        end
      end
      return true
    end
  end
end
