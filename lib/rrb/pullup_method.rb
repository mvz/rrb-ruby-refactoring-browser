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

    def check_pullup_method(namespace, node)
      return unless node.name == @method_name.name
      return unless namespace.match?(@old_namespace)
      
      subclass_info = @dumped_info[@old_namespace]
      node.calls.each do |call|
        if subclass_info.has_method?(MethodName.new(call.name), false)
          @result = false
          @error_message = "#{@old_namespace.name}##{@method_name.name} uses #{call.name} defined at #{@old_namespace.name}\n"
        end
      end
    end

    def visit_method(namespace, node)
      return unless @method_name.instance_method?
      check_pullup_method(namespace, node)
    end

    def visit_class_method(namespace, node)
      return unless @method_name.class_method?
      check_pullup_method(namespace, node)
    end
  end

  class ScriptFile

    def pullup_method(old_namespace, method_name, new_namespace, 
                      pullupped_method, ignore_new_namespace, specified_lineno)
      visitor = MoveMethodVisitor.new(old_namespace, method_name,
                                      new_namespace,
                                      ignore_new_namespace, specified_lineno)
      @tree.accept( visitor )
      if method_name.class_method?
        pullupped_method.gsub!(/^((\s)*def\s+)(.*)\./) {|s| $1 + new_namespace.name + '.'}
      end
      @new_script = RRB.insert_str(@input, visitor.insert_lineno,
                                   visitor.delete_range, pullupped_method,
                                   true)
    end

    def pullup_method?(dumped_info, old_namespace, method_name, new_namespace)
      visitor = PullupMethodCheckVisitor.new(dumped_info, old_namespace,
                                             method_name, new_namespace)
      @tree.accept(visitor)
      @error_message = visitor.error_message unless visitor.result
      return visitor.result
    end
  end

  class Script
    def pullup_method(old_namespace, method_name, new_namespace,
                      path, lineno)
      pullupped_method = get_string_of_method(old_namespace, method_name)
      @files.each do |scriptfile|
	scriptfile.pullup_method(old_namespace, method_name,
                                 new_namespace, pullupped_method,
                                 scriptfile.path != path,
                                 lineno)
      end      
    end

    def pullup_method?(old_namespace, method_name, new_namespace,
                       path, lineno)
      unless get_dumped_info[old_namespace].has_method?(method_name, false)
        @error_message = "#{method_name.name}: no definition at #{old_namespace.name}\n"
        return false
      end

      unless get_dumped_info[old_namespace].subclass_of?(new_namespace)
        @error_message = "#{new_namespace.name} is not the superclass of #{old_namespace.name}\n"
        return false
      end

      if get_dumped_info[new_namespace].has_method?(method_name)
        @error_message = "#{method_name.name}: already defined at #{new_namespace.name}\n"
        return false
      end

      definition_count = count_namespace_definition(path, new_namespace)
      if definition_count == 0
        @error_message = "No definition of #{new_namespace.name} in #{path}\n"
        return false
      elsif definition_count > 1
        target_class = get_class_on_cursor(path, lineno)
        unless target_class && new_namespace.contain?(target_class.normal )
          @error_message = "Specify which definition to pull up method to\n"
          return false
        end
      end


      @files.each do |scriptfile|
        unless scriptfile.pullup_method?(get_dumped_info, old_namespace, method_name, new_namespace)
          @error_message = scriptfile.error_message
          return false          
        end
      end

      return true
    end
  end
end
