require 'rrb/script'
require 'stringio'

module RRB

  class ExtractMethodVisitor < Visitor

    def initialize(start_lineno, end_lineno)
      @start_lineno = start_lineno
      @end_lineno = end_lineno
      @method_lineno = 1
      @args = []
      @result = []
    end

    attr_reader :method_lineno, :args, :result
    
    def visit_node( namespace, node )
      vars = node.local_vars.map{|i| i.name}
      out_vars = []
      in_vars = []
      node.local_vars.each do |id|
        out_vars << id.name unless (@start_lineno..@end_lineno) === id.lineno
        in_vars << id.name if (@start_lineno..@end_lineno) === id.lineno
      end
      return if in_vars.empty?

      @args = out_vars & in_vars
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

    def visit_node( namespace, node )
      return if @end_lineno <= node.start_lineno
      return if node.end_lineno <= @start_lineno
      return if node.start_lineno < @start_lineno && @end_lineno < node.end_lineno
      @result = false
      
    end
    def visit_class(namespace, node)
#      return unless node.start_lineno < @start_lineno && @end_lineno < node.end_lineno
      node.method_defs.each do |defs|
        @result = false if defs.name == @new_method
      end
    end
  end

  def extract_method(src, new_method, start_lineno, end_lineno, method_lineno, args)
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
        dst << "\s" * def_space_num + "end\n"
      end
      if lineno == end_lineno
        dst << "\s" * call_space_num + "#{new_method}("
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
      @new_script = RRB.extract_method( StringIO.new(@new_script), new_method, start_lineno-1, end_lineno-1, visitor.method_lineno-1, visitor.args)
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
      return true
      @files.each do |scriptfile|
	if not scriptfile.extract_method?(new_method, start_lineno, end_lineno)
          return false
	end
      end
      return true
    end
  end
end
