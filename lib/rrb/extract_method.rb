require 'rrb/script'
require 'rrb/common_visitor'

require 'stringio'

module RRB

  class ExtractMethodVisitor < Visitor

    def initialize(start_lineno, end_lineno)
      @start_lineno = start_lineno
      @end_lineno = end_lineno
      @method_lineno = 1
      @args = []
      @assigned = []
    end

    attr_reader :method_lineno, :args, :assigned 

    def visit_method( namespace, node )
      return unless node.range.contain?(@start_lineno .. @end_lineno)
      vars = node.local_vars.map{|i| i.name}
      out_vars = []
      in_vars = []
      node.local_vars.each do |id|
        out_vars << id unless (@start_lineno..@end_lineno) === id.lineno
        in_vars << id if (@start_lineno..@end_lineno) === id.lineno
      end

      in_assigned = (node.assigned & in_vars)
      in_var_ref = in_vars - in_assigned
      @assigned = (node.assigned & in_vars).map{|i| i.name} & out_vars.map{|i| i.name}
      candidates = out_vars.map{|i| i.name} & in_vars.map{|i| i.name}
      candidates.each do |id|
        first_var_ref = in_var_ref.find{|i| i.name == id}
        first_assigned = in_assigned.find{|i| i.name == id}
        next unless first_var_ref
        unless first_assigned
          @args << id
          next
        end
  
        @args << id if first_var_ref.lineno <= first_assigned.lineno
      end
      @args.uniq!

      if node.name_id.name == 'toplevel'
        @method_lineno = @start_lineno
      else
        @method_lineno = node.name_id.lineno
      end
    end
  end

  module_function
  def fragment_of_call_method( new_method, args, assigned )
    if assigned.empty? then
      "#{new_method}(#{args.join(', ')})\n"
    else
      "#{assigned.join(', ')} = #{new_method}(#{args.join(', ')})\n"
    end
  end
  
  def fragment_of_def_new_method(new_method, args )
    "def #{new_method}(" + args.join(", ") + ")\n"
  end

  def lines_of_new_method(new_method, args, assigned, extracted )
    result = reindent_lines( extracted, INDENT_LEVEL )
    result.unshift fragment_of_def_new_method( new_method, args )
    unless assigned.empty? then
      result.push " "*INDENT_LEVEL + "return " + assigned.join(", ") + "\n"
    end
    result.push "end\n"
  end
  
  def extract_method(src, new_method, start_lineno, end_lineno, method_lineno, args, assigned)
    dst = ''

    lines = src.split(/^/)

    extracted = lines[start_lineno..end_lineno]
    def_space_num =  count_indent_str( lines[method_lineno] ) 
    call_space_num = count_indent( extracted )
    
    
    0.upto(lines.length-1) do |lineno|
      if lineno == method_lineno
        lines_of_def = lines_of_new_method( new_method, args, assigned, extracted )
        dst << reindent_lines( lines_of_def, def_space_num ).join
      end
      if lineno == end_lineno
        dst << "\s" * call_space_num
        dst << fragment_of_call_method( new_method, args, assigned )
      end
      unless (start_lineno..end_lineno) === lineno
        dst << lines[lineno]
      end
    end
    dst
  end

  class ScriptFile
    def extract_method(new_method, start_lineno, end_lineno)
      visitor = ExtractMethodVisitor.new(start_lineno, end_lineno) 
      @tree.accept( visitor )
      @new_script = RRB.extract_method( @input, new_method,
                                        start_lineno-1, end_lineno-1,
                                        visitor.method_lineno-1,
                                        visitor.args, visitor.assigned)
    end
  end

  class Script    
    def extract_method(path, new_method, start_lineno, end_lineno)
      @files.each do |scriptfile|
	next unless scriptfile.path == path
	scriptfile.extract_method(new_method, start_lineno, end_lineno )
      end
    end

    def extract_method?(path, new_method, start_lineno, end_lineno)
      unless RRB.valid_method?( new_method )
        @error_message = "#{new_method} is not a valid name for methods\n"
        return false
      end

      method = get_method_on_region(path, start_lineno..end_lineno)
      namespace = get_class_on_region(path, start_lineno..end_lineno)

      unless namespace && method
        @error_message = "please select statements\n"
        return false
      end

      if get_dumped_info[namespace.name].has_method?(new_method)
        @error_message = "#{namespace.name} already has #{new_method}\n"
        return false
      end
      
      return true
    end
  end
end
