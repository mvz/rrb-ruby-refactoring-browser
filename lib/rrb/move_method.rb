require 'rrb/script'

module RRB

  class MoveMethodVisitor < Visitor

    def initialize(method_name, old_namespace, new_namespace)
      @method_name = method_name
      @str_old_namespace = old_namespace.str
      @str_new_namespace = new_namespace.str
    end

    attr_reader :new_lineno, :old_start_lineno, :old_end_lineno
    
    def visit_class( namespace, node )
      str_namespace = NodeNamespace.new(node, namespace).str
      if @str_old_namespace == str_namespace
        node.method_defs.each do |method_def|
          if method_def.name == @method_name
            @old_start_lineno = method_def.head_keyword.lineno
            @old_end_lineno = method_def.tail_keyword.lineno
          end
        end
      end  
      if @str_new_namespace == str_namespace
        @new_lineno = node.name_id.lineno
      end
    end
  end
  
  class MoveMethodCheckVisitor < Visitor
    
    def initialize(method_name, old_namespace, new_namespace)
      @method_name = method_name
      @str_old_namespace = old_namespace.str
      @str_new_namespace = new_namespace.str
      @result = true
    end
    
    attr_reader :result
    
    def nodes_include_the_method?(nodes)
      nodes.map{|i| i.name}.include?(@method_name)
    end
    
    def class_include_the_method?(node)
      nodes_include_the_method?(node.method_defs) || nodes_include_the_method?(node.class_method_defs) || nodes_include_the_method?(node.singleton_method_defs)     
    end

    def visit_class(namespace, node)
      str_namespace = NodeNamespace.new(node, namespace).str
      if @str_old_namespace == str_namespace
        unless class_include_the_method? (node)
          @result = false 
        end 
      end
      if @str_new_namespace == str_namespace
        if class_include_the_method? (node)
          @result = false
        end
      end
    end
  end

  def move_method(src, old_start_lineno, old_end_lineno, new_lineno)

    dst = ''

    lines = src.readlines

    def_space_num =  /^(\s*)/.match(lines[new_lineno])[0].length + INDENT_LEVEL
    imp_space_num =  def_space_num + INDENT_LEVEL

    0.upto(lines.length-1) do |lineno|
      if lineno == new_lineno+1
        dst << "\s" * def_space_num + lines[old_start_lineno].lstrip
        (old_start_lineno+1..old_end_lineno-1).each do |i|
          dst << "\s" * imp_space_num + lines[i].lstrip
        end
        dst << "\s" * def_space_num + lines[old_end_lineno].lstrip
      end
      unless (old_start_lineno..old_end_lineno) === lineno
        dst << lines[lineno]
      end
    end
    dst
  end
  module_function :move_method

  class ScriptFile
    def move_method(method_name, old_namespace, new_namespace)
      visitor = MoveMethodVisitor.new(method_name, old_namespace, new_namespace)
      @tree.accept( visitor )
      @new_script = RRB.move_method( @input, visitor.old_start_lineno-1, visitor.old_end_lineno-1, visitor.new_lineno-1)
    end

    def move_method?(method_name, old_namespace, new_namespace)
      visitor = MoveMethodCheckVisitor.new(method_name, old_namespace, new_namespace)
      @tree.accept( visitor )
      return visitor.result
    end
  end

  class Script
    
    def move_method(method_name, old_namespace, new_namespace)
      @files.each do |scriptfile|
	scriptfile.move_method(method_name, old_namespace, new_namespace)
      end
    end

    def move_method?(method_name, old_namespace, new_namespace)
      @files.each do |scriptfile|
	if not scriptfile.move_method?(method_name, old_namespace, new_namespace)
          return false
	end
      end
      return true
    end
  end
end
