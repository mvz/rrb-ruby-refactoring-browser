require 'rrb/script'
require 'stringio'

module RRB

  class ExtractMethodGetNamespaceVisitor < Visitor
    def initialize(start_lineno, end_lineno)
      @start_lineno = start_lineno
      @end_lineno = end_lineno
      @namespace = @m_node = nil
    end
    attr_reader :namespace, :m_node

    def visit_toplevel(namespace, node)
      @m_node = node
      @namespace = namespace
    end

    def visit_method(namespace, node)
      if node.range.contain?( @start_lineno .. @end_lineno ) then
        @m_node = node
        @namespace = namespace
      else
        unless node.range.out_of?(@start_lineno .. @end_lineno) then
          @m_node = nil
          @namespace = nil
        end
      end
      
    end
    
  end
  
  class ExtractMethodVisitor < Visitor

    def initialize(start_lineno, end_lineno)
      @start_lineno = start_lineno
      @end_lineno = end_lineno
      @method_lineno = 1
      @args = []
      @assigned = []
    end

    attr_reader :method_lineno, :args, :assigned 

    def visit_node( namespace, node )
      vars = node.local_vars.map{|i| i.name}
      out_vars = []
      in_vars = []
      node.local_vars.each do |id|
        out_vars << id unless (@start_lineno..@end_lineno) === id.lineno
        in_vars << id if (@start_lineno..@end_lineno) === id.lineno
      end
      return if in_vars.empty?

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

  def space_width( str )
    result = 0
    str.each_byte do |c|
      if c == ?\t then
        result = (result/TAB_WIDTH + 1)*TAB_WIDTH
      else
        result += 1
      end
    end
    result
  end
  module_function :space_width
  
  def extract_method(src, new_method, start_lineno, end_lineno, method_lineno, args, assigned)
    dst = ''

    lines = src.split(/^/)
    
    def_space_num =  RRB.space_width(/^(\s*)/.match(lines[method_lineno])[0])
    call_space_num = RRB.space_width(/^(\s*)/.match(lines[start_lineno])[0])
    
    0.upto(lines.length-1) do |lineno|
      if lineno == method_lineno
        dst << "\s" * def_space_num + "def #{new_method}("
        dst << args.join(", ")
        dst << ")\n"
        for i in start_lineno..end_lineno
          space_num = lines[i][/^\s*/].length 
          imp_space_num = def_space_num + space_num - call_space_num + 2
          dst << "\s" * imp_space_num + /^(\s*)(.*)/.match(lines[i])[2] + "\n"
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
    def get_emethod_namespace(start_lineno, end_lineno)
      get_namespace = ExtractMethodGetNamespaceVisitor.new(start_lineno, end_lineno)
      @tree.accept(get_namespace)
      get_namespace.namespace
    end
    
    def extract_method(new_method, start_lineno, end_lineno)
      visitor = ExtractMethodVisitor.new(start_lineno, end_lineno) 
      @tree.accept( visitor )
      @new_script = RRB.extract_method( @input, new_method, start_lineno-1, end_lineno-1, visitor.method_lineno-1, visitor.args, visitor.assigned)
    end
  end

  class Script

    
    def get_real_emethod_owner(namespace, new_method)
      @files.inject(namespace ) do |owner,scriptfile|
	scriptfile.get_ancestral_emethod_owner(owner, get_dumped_info, new_method)
      end
    end    
    
    def extract_method(path, new_method, start_lineno, end_lineno)
      @files.each do |scriptfile|
	next unless scriptfile.path == path
	scriptfile.extract_method(new_method, start_lineno, end_lineno )
      end
    end

    def extract_method?(path, new_method, start_lineno, end_lineno)
      namespace = ""
      @files.each do |scriptfile|
	next unless scriptfile.path == path
        namespace = scriptfile.get_emethod_namespace(start_lineno, end_lineno)
      end
      return false unless namespace
      return false if get_dumped_info[namespace.str].has_method?(new_method)
      
      return true
    end
  end
end
