require 'rrb/script'

module RRB

  class RenameGlobalVarVisitor < Visitor

    def initialize(old_var, new_var )
      @old_var = old_var
      @new_var = new_var
      @result = []
    end

    attr_reader :result

    def visit_node( namespace, node)
      node.global_vars.each do |id|
        if id.name == @old_var then
          @result << Replacer.new_from_id( id, @new_var )
        end
      end
    end
  end

  class RenameGlobalVarCheckVisitor < Visitor
    
    def initialize(old_var, new_var )
      @old_var = old_var
      @new_var = new_var
      @result = true
    end

    attr_reader :result

    def visit_node( namespace, node )
      if node.global_vars.find{|i| i.name == @new_var} then
        @error_message = "#{@new_var} is already used\n"
	@result = false
      end
    end
    
  end

  class ScriptFile
    
    def rename_global_var(old_var, new_var )
      visitor = RenameGlobalVarVisitor.new( old_var, new_var )
      @tree.accept( visitor )
      unless visitor.result.empty? then
	@new_script = RRB.replace_str( @input, visitor.result )
      end
    end

    def rename_global_var?(old_var, new_var )

      visitor = RenameGlobalVarCheckVisitor.new( old_var, new_var )
      @tree.accept( visitor )
      @error_message = visitor.error_message unless visitor.result
      return visitor.result
    end

  end

  class Script
    def rename_global_var(old_var, new_var )
      @files.each do |scriptfile|
	scriptfile.rename_global_var(old_var, new_var )
      end
    end

    def rename_global_var?(old_var, new_var )
      unless RRB.valid_global_var?( new_var )
        @error_message = "#{new_var} is not a valid name for global variables\n"
        return false
      end

      @files.each do |scriptfile|
	if not scriptfile.rename_global_var?(old_var, new_var ) then
          @error_message = scriptfile.error_message
	  return false
	end
      end
      return true
    end

  end
end
