require 'rrb/script'
require 'stringio'

module RRB

  class ExtractMethodVisitor < Visitor

    def initialize(start_lineno, end_lineno)
      @start_lineno = start_lineno
      @end_lineno = end_lineno
      @method_lineno = 1
      @args = []
      @assigned = []
      @result = []
    end

    attr_reader :method_lineno, :args, :assigned, :result
    
    def visit_node( namespace, node )
      vars = node.local_vars.map{|i| i.name}
      out_vars = []
      in_vars = []
      node.local_vars.each do |id|
        out_vars << id unless (@start_lineno..@end_lineno) === id.lineno
        in_vars << id if (@start_lineno..@end_lineno) === id.lineno
      end
      return if in_vars.empty?

      @args = out_vars.map{|i| i.name} & in_vars.map{|i| i.name}
      @assigned = (node.assigned & in_vars).map{|i| i.name} & out_vars.map{|i| i.name}
      
      if node.name_id.name == 'toplevel'
        @method_lineno = @start_lineno
      else
        @method_lineno = node.name_id.lineno
      end
    end
  end

  class ExtractMethodCheckVisitor < Visitor
    
    def initialize(new_method, start_lineno, end_lineno)
      @new_method = new_method
      @start_lineno = start_lineno
      @end_lineno = end_lineno
      @result = true
    end

    attr_reader :result

    def toplevel?(node)
      return node.name_id.type == :toplevel
    end

    def in_lines?(node)
      return node.head_keyword.lineno < @start_lineno && @end_lineno < node.tail_keyword.lineno
    end
    def out_lines?(node)      
      return @end_lineno < node.head_keyword.lineno || node.tail_keyword.lineno < @start_lineno

    end


    def visit_toplevel(namespace, node)
      @str_namespace = get_namespace(node)
    end

    def visit_node(namespace, node)
      return if toplevel?(node)
      return if in_lines?(node)
      return if out_lines?(node)
      @result = false
    end

    def visit_class(namespace, node)
      str_namespace = namespace.map{|i| i.name}.join('::')
      if str_namespace.empty?
        str_namespace = node.name
      else
        str_namespace = str_namespace + '::' + node.name
      end
      if @str_namespace == str_namespace
        node.method_defs.each do |defs|
          @result = false if defs.name == @new_method
        end
      end
    end

    def get_namespace(node)
      unless node.class_defs.empty?
        node.class_defs.each do |class_def|
          str_namespace = get_namespace(class_def)
          unless str_namespace.nil?
            if toplevel?(node)
              return str_namespace
            else
              return node.name +  '::' + str_namespace
            end
          end
        end
      else
        if toplevel?(node)
          return ""
        elsif in_lines?(node)
          return node.name
        end
      end
    end
  end

  def extract_method(src, new_method, start_lineno, end_lineno, method_lineno, args, assigned)
    dst = ''

    lines = src.readlines

    imp_space_num =  /^(\s*)/.match(lines[start_lineno])[0].length
    if imp_space_num < 2
      def_space_num = 0
      offset_space_num = 2
      call_space_num = 0
    else
      def_space_num = imp_space_num - 2
      call_space_num = imp_space_num
      offset_space_num = 0
    end

    0.upto(lines.length-1) do |lineno|
      if lineno == method_lineno
        dst << "\s" * def_space_num + "def #{new_method}("
        dst << args.join(", ")
        dst << ")\n"
        for i in start_lineno..end_lineno
          dst << "\s" * offset_space_num + lines[i]
        end
        unless assigned.empty?
          dst << "\s" * imp_space_num + "return " + assigned.join(", ") + "\n"
        end
        dst << "\s" * def_space_num + "end\n"
      end
      if lineno == end_lineno
        dst << "\s" * call_space_num
        unless assigned.empty?
          dst << assigned.join(", ") + " = " 
        end
        dst << "#{new_method}("
        dst << args.join(", ")
        dst << ")\n"
      end
      unless (start_lineno..end_lineno) === lineno
        dst << lines[lineno]
      end
    end
    dst
  end
  module_function :extract_method

  class ScriptFile
    def extract_method(new_method, start_lineno, end_lineno)
      visitor = ExtractMethodVisitor.new(start_lineno, end_lineno) 
      @tree.accept( visitor )
      @new_script = RRB.replace_str( @input, visitor.result )
      @new_script = RRB.extract_method( StringIO.new(@new_script), new_method, start_lineno-1, end_lineno-1, visitor.method_lineno-1, visitor.args, visitor.assigned)
    end

    def extract_method?(new_method, start_lineno, end_lineno)
      return false unless RRB.valid_method?(new_method)
      visitor = ExtractMethodCheckVisitor.new(new_method, start_lineno, end_lineno)
      @tree.accept( visitor )
      return visitor.result
    end

  end

  class Script
    
    def extract_method(new_method, start_lineno, end_lineno)
      @files.each do |scriptfile|
	scriptfile.extract_method(new_method, start_lineno, end_lineno)
      end
    end

    def extract_method?(new_method, start_lineno, end_lineno)
      @files.each do |scriptfile|
	if not scriptfile.extract_method?(new_method, start_lineno, end_lineno)
          return false
	end
      end
      return true
    end
  end
end
