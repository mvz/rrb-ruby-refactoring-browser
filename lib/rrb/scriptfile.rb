require 'rrb/parser'

module RRB

  class RenameLocalVarVisitor < Visitor

    def initialize( namespace, method_name, old_var, new_var )
      @namespace = namespace
      @method_name = method_name
      @old_var = old_var
      @new_var = new_var
      @result = []
    end

    attr_reader :result
    
    def visit_method( namespace, method_node )
      unless method_node.name == @method_name &&
	  namespace.map{|i| i.name} == @namespace then
	return
      end
	
      method_node.local_vars.each do |id|
	if id.name == @old_var then
	  @result <<
	    Replacer.new( id.lineno, id.pointer, @old_var, @new_var )
	end
      end
      
    end
    
  end

  class RenameLocalVarCheckVisitor < Visitor
    
    def initialize( namespace, method_name, old_var, new_var )
      @namespace = namespace
      @method_name = method_name
      @old_var = old_var
      @new_var = new_var
      @result = true
    end

    attr_reader :result

    def visit_method( namespace, method_node )
      unless method_node.name == @method_name &&
	  namespace.map{|i| i.name} == @namespace then
	return
      end
	
      if method_node.local_vars.find{|i| i.name == @new_var} then
	@result = false
      end
      if method_node.fcalls.find{|i| i.name == @new_var} then
	@result = false
      end
      
    end
    
  end
  
  class ScriptFile

    def initialize( input, name )
      @input = input
      @name = name
      @tree = Parser.new.run( input )
      input.rewind 
      @new_script = nil
    end
    
    def rename_local_var( namespace, method_name, old_var, new_var )
      visitor = RenameLocalVarVisitor.new( namespace, method_name,
					  old_var, new_var )
      @tree.accept( visitor )
      @new_script = RRB.replace_str( @input, visitor.result )
    end

    def rename_local_var?( namespace, method_name, old_var, new_var )
      return false unless RRB.valid_local_var?( new_var )
      visitor = RenameLocalVarCheckVisitor.new( namespace, method_name,
					       old_var, new_var )
      @tree.accept( visitor )
      return visitor.result
    end
    
    attr_reader :new_script, :name

  end


  Replacer = Struct.new( :lineno, :pointer, :before, :after )

  # guard object 
  Guard = Object.new
  def Guard.lineno
    -1
  end
  
  module_function
  
  def replace_str( src, replace_info )
    sorted_info = replace_info.sort_by{|i| [ i.lineno, -i.pointer ] }
    sorted_info << Guard
    
    info_index = 0
    dst = ''
    line = src.gets
    
    while line 
      
      if src.lineno == sorted_info[info_index].lineno then
	info = sorted_info[info_index]

	line[ info.pointer-info.before.size, info.before.size ] = info.after
	info_index += 1
      else
	dst << line
	line = src.gets
      end
      
    end

    return dst
  end

  def valid_local_var?( id )
    /^[a-z_][a-zA-Z0-9_]*$/ =~ id
  end

  def valid_const_var?( id )
    /^[A-Z][a-zA-Z0-9_]*$/ =~ id
  end
 

end
