require 'rrb/script'

module RRB

  class GetInstanceVarOwnerVisitor < Visitor
    def initialize(namespace, dumped_info, old_var)
      @old_var = old_var
      @dumped_info = dumped_info
      @my_info = dumped_info[namespace]
      @owner = namespace
    end

    attr_reader :owner
    
    def visit_method(namespace, node)
      return unless node.instance_vars.find{|i| i.name == @old_var}
      ancestor_names = @dumped_info[@owner].ancestor_names
      new_owner = ancestor_names.find{|anc| anc == namespace.normal}
      @owner = new_owner if new_owner
    end
  end

  class RenameInstanceVarVisitor < Visitor

    def initialize( owner, dumped_info, old_var, new_var )
      @owner = owner
      @old_var = old_var
      @new_var = new_var
      @dumped_info = dumped_info
      @result = []
    end

    attr_reader :result

    
    def check_namespace(namespace)
      @dumped_info[namespace.normal].subclass_of?(@owner)
    end

    def rename_instance_var(namespace, node)
      if check_namespace(namespace)
        node.instance_vars.each do |id|
          if id.name == @old_var then
            @result << Replacer.new_from_id( id, @new_var )
          end
        end  
      end
    end

    def visit_method( namespace, node )
      rename_instance_var( namespace, node)
    end
  end


  class RenameInstanceVarCheckVisitor < Visitor
    
    def initialize( owner, dumped_info, old_var, new_var )
      @owner = owner
      @dumped_info = dumped_info
      @old_var = old_var
      @new_var = new_var
      @result = true
    end

    attr_reader :result

    def check_namespace(namespace)
      return @dumped_info[namespace.normal].subclass_of?(@owner)
    end

    def rename_instance_var?(namespace, node)
      if check_namespace(namespace)
        node.instance_vars.each do |id|
          if id.name == @new_var then
            return false
          end
        end  
      end
      return true
    end

    def visit_method( namespace, node )
      if !rename_instance_var?( namespace, node)
        @result = false
      end
    end
  end

  class ScriptFile

    def get_ancestral_ivar_owner( namespace, dumped_info, var )
      get_owner = GetInstanceVarOwnerVisitor.new(namespace, dumped_info, var)
      @tree.accept(get_owner)
      get_owner.owner
    end
    
    def rename_instance_var( real_owner, dumped_info, old_var, new_var )
      visitor = RenameInstanceVarVisitor.new( real_owner, dumped_info,
					  old_var, new_var )
      @tree.accept( visitor )
      unless visitor.result.empty? then
	@new_script = RRB.replace_str( @input, visitor.result )
      end
    end

    def rename_instance_var?( real_owner, dumped_info, old_var, new_var )
      return false unless RRB.valid_instance_var?( new_var )
      visitor = RenameInstanceVarCheckVisitor.new( real_owner, dumped_info,
						  old_var, new_var )
      @tree.accept( visitor )
      return visitor.result
    end

  end

  class Script

    def get_real_ivar_owner( namespace, var )
      @files.inject( namespace ) do |owner,scriptfile|
	scriptfile.get_ancestral_ivar_owner( owner, get_dumped_info, var )
      end
    end
    
    def rename_instance_var( namespace, old_var, new_var )

      owner = get_real_ivar_owner( namespace, old_var )
      @files.each do |scriptfile|
	scriptfile.rename_instance_var( owner, get_dumped_info,
				       old_var, new_var )
      end
    end

    def rename_instance_var?( namespace, old_var, new_var )
      
      owner = get_real_ivar_owner( namespace, old_var )
      @files.each do |scriptfile|
	if not scriptfile.rename_instance_var?( owner, get_dumped_info,
					    old_var, new_var ) then
	  return false
	end
      end

      return true
      
    end

  end
  
end
