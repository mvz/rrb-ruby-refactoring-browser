require 'rrb/scriptfile'
require 'rrb/script'
require 'rrb/node.rb'
require 'rrb/parser.rb'
require 'rrb/common_visitor'

module RRB
  
  class PullupMethodVisitor < Visitor

    def initialize(old_namespace, method_name, new_namespace)
      @method_name = method_name
      @old_namespace = old_namespace
      @new_namespace = new_namespace
      @subclass_range = nil
      @superclass_lineno = nil
    end

    attr_reader :subclass_range, :superclass_lineno

    def visit_class(namespace, node)
      cur_namespace = NodeNamespace.new(node, namespace)
      if cur_namespace.match?(@new_namespace) 
        @superclass_lineno = node.range.head.lineno
      elsif cur_namespace.match?(@old_namespace)
        node.method_defs.each do |method|
          if method.name == @method_name 
            @subclass_range = method.range
          end
        end
      end
    end
  end

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
    
  def pullup_method(src, superclass_lineno, subclass_range, pullupped_method)
    dst = ''
    lines = src.split(/^/)

    0.upto(lines.length - 1) do |lineno|
      if lineno == superclass_lineno
        dst << pullupped_method.join
      end
      if subclass_range
        unless (subclass_range.head.lineno-1..subclass_range.tail.lineno-1) === lineno
          dst << lines[lineno]
        end
      else
        dst << lines[lineno]
      end
    end
    dst
  end
  module_function :pullup_method

  class ScriptFile

    def get_targetmethod(namespace, method_name)
      visitor = GetTargetMethodVisitor.new(namespace, method_name)
      @tree.accept(visitor)
      range = visitor.result_range
      if range
        return @input.split(/^/)[range.head.lineno-1..range.tail.lineno-1]
      else
        return nil
      end
    end

    def pullup_method(old_namespace, method_name, new_namespace, pullupped_method)
      visitor = PullupMethodVisitor.new(old_namespace, method_name,  new_namespace)
      @tree.accept( visitor )
      @new_script = RRB.pullup_method(@input, visitor.superclass_lineno, visitor.subclass_range, pullupped_method)
    end

    def pullup_method?(dumped_info, old_namespace, method_name, new_namespace)
      visitor = PullupMethodCheckVisitor.new(dumped_info, old_namespace, method_name, new_namespace)
      @tree.accept(visitor)
      return visitor.result
    end
  end

  class Script
    def pullup_method(old_namespace, method_name, new_namespace)
      pullupped_method = nil
      @files.each do |scriptfile|
        pullupped_method = pullupped_method || scriptfile.get_targetmethod(old_namespace, method_name)
      end
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
