require 'rrb/scriptfile'
require 'rrb/script'
require 'rrb/node.rb'
require 'rrb/parser.rb'
require 'rrb/common_visitor.rb'

module RRB
  
  class PushdownMethodVisitor < Visitor

    def initialize(method_name, old_namespace, new_namespace)
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
    def initialize(dumped_info, method_name, old_namespace, new_namespace)
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

    
  def pushdown_method(src, superclass_range, subclass_lineno, pushdowned_method)
    dst = ''
    lines = src.split(/^/)

    0.upto(lines.length - 1) do |lineno|
      if lineno == subclass_lineno
        dst << pushdowned_method.join
      end
      if superclass_range
        unless (superclass_range.head.lineno-1..superclass_range.tail.lineno-1) === lineno
          dst << lines[lineno]
        end
      else
        dst << lines[lineno]
      end
    end
    dst
  end
  module_function :pushdown_method

  class ScriptFile

    def get_targetmethod(method_name, namespace)
      visitor = GetTargetMethodVisitor.new(method_name, namespace)
      @tree.accept(visitor)
      range = visitor.result_range
      if range
        return @input.split(/^/)[range.head.lineno-1..range.tail.lineno-1]
      else
        return nil
      end
    end

    def pushdown_method(method_name, old_namespace, new_namespace, pushdowned_method)
      visitor = PushdownMethodVisitor.new(method_name, old_namespace, new_namespace)
      @tree.accept( visitor )
      @new_script = RRB.pushdown_method(@input, visitor.superclass_range, visitor.subclass_lineno, pushdowned_method)
    end

    def pushdown_method?(dumped_info, method_name, old_namespace, new_namespace)
      visitor = PushdownMethodCheckVisitor.new(dumped_info, method_name, old_namespace, new_namespace)
      @tree.accept(visitor)
      return visitor.result
    end
  end

  class Script
    def pushdown_method(method_name, old_namespace, new_namespace)
      pushdowned_method = nil
      @files.each do |scriptfile|
        pushdowned_method = pushdowned_method || scriptfile.get_targetmethod(method_name, old_namespace)
      end
      @files.each do |scriptfile|
	scriptfile.pushdown_method(method_name, old_namespace, new_namespace, pushdowned_method)
      end      
    end

    def pushdown_method?(method_name, old_namespace, new_namespace)
      return false unless get_dumped_info[old_namespace].has_method?(method_name, false)
      return false if get_dumped_info[new_namespace].has_method?(method_name, false)
      @files.each do |scriptfile|
        unless scriptfile.pushdown_method?(get_dumped_info, method_name, old_namespace, new_namespace)
          return false
        end
      end
      return true
    end
  end
end
