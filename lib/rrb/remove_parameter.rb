require 'rrb/script'
require 'rrb/common_visitor'

require 'stringio'

module RRB

  class GetParameterIndexVisitor < Visitor
    def initialize(namespace, method_name, target_parameter)
      @namespace = namespace
      @method_name = method_name
      @target_parameter = target_parameter
      @parameter_index = nil
    end

    attr_reader :parameter_index

    def visit_method( namespace, node )
      if namespace.match?( @namespace ) &&  @method_name == node.name
        @parameter_index = node.args.map{|arg| arg.name}.index(@target_parameter)
      end
    end
  end



  class RemoveParameterVisitor < Visitor

    def initialize(namespace, method_name, parameter_index)
      @namespace = namespace
      @method_name = method_name
      @parameter_index = parameter_index
      @result = []
    end

    attr_reader :result

    def remove_method_def_parameter(node)
      remove_arg = node.args[@parameter_index]
      @result << Replacer.new_from_id(remove_arg, '' )

    end

    def remove_fcall_parameter( fcall )
      remove_arg = fcall.args[@parameter_index]
      if remove_arg
        @result << Replacer.new_from_id(remove_arg, '')
      end
    end
    
    def visit_method( namespace, node )
      
      if namespace.match?( @namespace ) &&  @method_name == node.name
        remove_method_def_parameter(node)
      end
      if namespace.match?( @namespace )
        node.fcalls.each do|fcall|
          if fcall.body.name == @method_name
            remove_fcall_parameter(fcall)
          end
        end
      end
    end
  end

  class RemoveParameterCheckVisitor < Visitor

    def initialize(namespace, method_name, target_parameter)
      @namespace = namespace
      @method_name = method_name
      @target_parameter = target_parameter
      @result = true
    end

    def visit_method(namespace, node)
      if namespace.match?(@namespace) 
        if @method_name == node.name
          unless node.args.map{|arg| arg.name}.include?(@target_parameter)
            @error_message = "#{@target_parameter}: no such parameter\n"
            @result = false
          end
          
          if node.local_vars.map{|local_var| local_var.name}.find_all{|var_name|
              var_name == @target_parameter}.size >= 2
            @error_message = "#{@target_parameter} is used\n"
            @result = false
          end
        end

        node.fcalls.find_all{|fcall| fcall.name == @method_name}.each do |fcall|
          if fcall.args.include?(nil) || fcall.args == []
            @error_message = "parameter is too complex\n"
            @result = false
          end
        end
      end
    end
    
    attr_reader :result
  end
  
  

  class ScriptFile
    def get_parameter_index(namespace, method_name, target_parameter)
      visitor = GetParameterIndexVisitor.new(namespace, method_name,
                                           target_parameter) 
      @tree.accept( visitor )
      return visitor.parameter_index
    end

    def remove_parameter(namespace, method_name, parameter_index)
      visitor = RemoveParameterVisitor.new(namespace, method_name,
                                           parameter_index) 
      @tree.accept( visitor )
      @new_script = RRB.replace_str( @input, visitor.result )
    end

    def remove_parameter?(namespace, method_name, target_parameter)
      visitor = RemoveParameterCheckVisitor.new(namespace, method_name,
                                                target_parameter)
      @tree.accept( visitor )
      @error_message = visitor.error_message unless visitor.result
      return visitor.result
    end
  end

  class Script    
    def get_parameter_index(namespace, method_name, target_parameter)
      @files.inject(nil) do |parameter_index, scriptfile|
        parameter_index ||= scriptfile.get_parameter_index(namespace, 
                                                           method_name,
                                                           target_parameter)
      end
    end

    def remove_parameter(namespace, method_name, target_parameter)
      parameter_index = get_parameter_index(namespace, method_name,
                                            target_parameter)
      @files.each do |scriptfile|
	scriptfile.remove_parameter(namespace, method_name, parameter_index)
      end
    end
    
    def remove_parameter?(namespace, method_name, target_parameter)
      unless get_dumped_info[namespace.name].has_method?(method_name, false)
        @error_message = "#{method_name} isn't defined at #{namespace.name}\n"
        return false
      end

      @files.each do |scriptfile|
        unless scriptfile.remove_parameter?(namespace, method_name,
                                            target_parameter)
          @error_message = scriptfile.error_message
          return false          
        end
      end
      
      return true
    end
  end
end
