require 'rrb/scriptfile'
require 'rrb/script'
require 'rrb/node.rb'
require 'rrb/parser.rb'
require 'rrb/common_visitor.rb'

module RRB
  
  class PushdownMethodCheckVisitor < Visitor
    def initialize(dumped_info, method_name, new_namespace)
      @dumped_info = dumped_info
      @method_name = method_name
      @old_namespace = method_name.namespace
      @new_namespace = new_namespace
      @result = true
    end

    attr_reader :result

    def check_pushdown_method(namespace, node)

      return if @method_name.match_node?( namespace, node )
      return unless @dumped_info[namespace.normal].subclass_of?(@old_namespace)
      return if @dumped_info[namespace.normal].subclass_of?(@new_namespace)
      return unless node.fcalls.any?{|fcall| fcall.name == @method_name.str_method_name}
      called_method = MethodName.new( namespace.normal, @method_name.str_method_name )
      if @dumped_info.real_method( called_method ) == @method_name
        @result = false
        @error_message = "#{namespace.name} calls #{@method_name.name}"
      end
      
    end

    def visit_method(namespace, node)
      return unless @method_name.instance_method?
      check_pushdown_method(namespace, node)
    end

    def visit_class_method(namespace, node)
      return unless @method_name.class_method?
      check_pushdown_method(namespace, node)
    end
  end

    
  class ScriptFile

    def pushdown_method( method_name, new_namespace, 
                        pushdowned_method,
                        ignore_new_namespace, specified_lineno)
      visitor = MoveMethodVisitor.new(method_name,
                                      new_namespace, 
                                      ignore_new_namespace, specified_lineno)
      @tree.accept( visitor )
      if method_name.class_method?
        pushdowned_method.gsub!(/^((\s)*def\s+)(.*)\./) {|s| $1 + new_namespace.name + '.'}
      end
      @new_script = RRB.insert_str(@input, visitor.insert_lineno,
                                   visitor.delete_range, pushdowned_method, true)
    end

    def pushdown_method?(dumped_info, method_name, new_namespace)
      visitor = PushdownMethodCheckVisitor.new(dumped_info,
                                               method_name, new_namespace)
      @tree.accept(visitor)
      @error_message = visitor.error_message unless visitor.result 
      return visitor.result
    end
  end

  class Script
    def pushdown_method(method_name, new_namespace,
                        path, lineno)
      pushdowned_method = get_string_of_method( method_name)
      @files.each do |scriptfile|
	scriptfile.pushdown_method(method_name,
                                   new_namespace, pushdowned_method, 
                                   scriptfile.path != path,
                                   lineno)
      end      
    end

    def pushdown_method?(method_name, new_namespace,
                         path, lineno)
      old_namespace = method_name.namespace
      unless get_dumped_info.exist?(method_name)
        @error_message = "#{method_name.name}: no definition in #{old_namespace.name}\n"
        return false
      end

      unless get_dumped_info[new_namespace].subclass_of?(method_name.namespace)
        @error_message = "#{new_namespace.name} is not the subclass of #{old_namespace.name}\n"
        return false
      end

      if get_dumped_info[new_namespace].has_method?(method_name.str_method_name, false)
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
          @error_message = "Specify which definition to push down method to\n"
          return false
        end
      end


      @files.each do |scriptfile|
        unless scriptfile.pushdown_method?(get_dumped_info,
                                           method_name, new_namespace)
          @error_message = scriptfile.error_message
          return false          
        end
      end

      return true
    end
  end
end
