require 'rrb/script'

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
      unless method_node.name == @method_name && namespace.match?( @namespace ) then
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
	  namespace.match?( @namespace ) then
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

  end

  class Script
    
    def rename_local_var( namespace, method_name, old_var, new_var )
      @files.each do |scriptfile|
	scriptfile.rename_local_var( namespace, method_name,
				    old_var, new_var )
      end
    end

    def rename_local_var?( namespace, method_name, old_var, new_var )
      @files.each do |scriptfile|
	if not scriptfile.rename_local_var?( namespace, method_name,
					    old_var, new_var ) then
	  return false
	end
      end

      return true
      
    end

  end
  
end
