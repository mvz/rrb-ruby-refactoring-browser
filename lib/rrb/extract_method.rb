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
      @namespace = namespace
      @m_node = node
    end
    
    def visit_method(namespace, node)
      if node.range.contain?( @start_lineno .. @end_lineno ) then
        @m_node = node
        @namespace = namespace
      end
    end
    
  end
  
  class ExtractMethodOwnerVisitor < Visitor
    def initialize(namespace, dumped_info, new_method)
      @new_method = new_method
      @dumped_info = dumped_info
      @my_info = dumped_info[namespace.str]
      @owner = namespace
    end

    attr_reader :owner
    
    def visit_class(namespace, node)
      class_node = NodeNamespace.new(node, namespace)
      ancestor_names = @dumped_info[@owner.str].ancestor_names
      new_owner = class_node if ancestor_names.find{|anc| anc == class_node.str}
      @owner = new_owner if new_owner
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

  class ExtractMethodCheckVisitor < Visitor
    
    def initialize(owner, dumped_info, new_method, start_lineno, end_lineno)
      @owner = owner
      @dumped_info = dumped_info
      @new_method = new_method
      @start_lineno = start_lineno
      @end_lineno = end_lineno
      @result = true
    end

    attr_reader :result

    def toplevel?(node)
      node.kind_of?( TopLevelNode )
    end

    def in_lines?(node)
      node.range.contain?( @start_lineno .. @end_lineno )
    end
    
    def out_lines?(node)
      node.range.out_of?( @start_lineno .. @end_lineno )
    end


    def visit_node(namespace, node)
      return if toplevel?(node)
      return if in_lines?(node)
      return if out_lines?(node)
      @result = false
    end

    def visit_class(namespace, node)
      if @dumped_info[NodeNamespace.new(node, namespace).normal].subclass_of?(@owner.normal)
        node.method_defs.each do |defs|
          @result = false if defs.name == @new_method
        end
        node.local_vars.each do |var|
          @result = false if var.name == @new_method
        end
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

    lines = src.readlines

    imp_space_num =  RRB.space_width(/^(\s*)/.match(lines[start_lineno])[0])
    if imp_space_num < INDENT_LEVEL
      def_space_num = 0
      offset_space_num = INDENT_LEVEL
      call_space_num = 0
    else
      def_space_num = imp_space_num - INDENT_LEVEL
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
    def get_emethod_namespace(start_lineno, end_lineno)
      get_namespace = ExtractMethodGetNamespaceVisitor.new(start_lineno, end_lineno)
      @tree.accept(get_namespace)
      get_namespace.namespace
    end

    
    def get_ancestral_emethod_owner(namespace, dumped_info, new_method )
      get_owner = ExtractMethodOwnerVisitor.new(namespace, dumped_info, new_method)
      @tree.accept(get_owner)
      get_owner.owner
    end
    
    def extract_method(new_method, start_lineno, end_lineno)
      visitor = ExtractMethodVisitor.new(start_lineno, end_lineno) 
      @tree.accept( visitor )
      @new_script = RRB.extract_method( @input, new_method, start_lineno-1, end_lineno-1, visitor.method_lineno-1, visitor.args, visitor.assigned)
    end

    def extract_method?(str_owner, dumped_info, new_method, start_lineno, end_lineno)
      return false unless RRB.valid_method?(new_method)
      visitor = ExtractMethodCheckVisitor.new(str_owner, dumped_info, new_method, start_lineno, end_lineno)
      @tree.accept( visitor )
      return visitor.result
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
      owner = get_real_emethod_owner(namespace, new_method)
      return false unless owner
      
      @files.each do |scriptfile|
	next unless scriptfile.path == path
	if not scriptfile.extract_method?(owner, get_dumped_info, new_method, start_lineno, end_lineno)
          return false
	end
      end
      return true
    end
  end
end
