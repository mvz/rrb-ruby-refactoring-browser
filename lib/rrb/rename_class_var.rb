require 'rrb/script'

module RRB

  class GetClassVarOwnerVisitor < Visitor
    def initialize(namespace, dumped_info, old_var)
      @old_var = old_var
      @dumped_info = dumped_info
      @my_info = dumped_info[namespace]
      @owner = namespace
    end

    attr_reader :owner
   
    def visit_class(namespace, node)
      return false unless node.class_vars.find{|i| i.name == @old_var}
      ancestor_names = @dumped_info[@owner].ancestor_names
      class_name = NodeNamespace.new( node, namespace ).normal
      new_owner = ancestor_names.find{|anc| anc == class_name}
      @owner = new_owner if new_owner
    end
  end

  class RenameClassVarVisitor < Visitor

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

    def rename_class_var(namespace, node)
      if check_namespace(namespace)
        node.class_vars.each do |id|
          if id.name == @old_var then
            @result << Replacer.new_from_id( id, @new_var )
          end
        end  
      end
    end

    def visit_method( namespace, node )
      rename_class_var(namespace, node)
    end

    def visit_class_method(namespace, node)
      rename_class_var(namespace, node)
    end

    def visit_class(namespace, node)
      rename_class_var(NodeNamespace.new( node, namespace ), node)
    end
  end


  class RenameClassVarCheckVisitor < Visitor
    
    def initialize(owner, dumped_info, old_var, new_var )
      @owner = owner
      @dumped_info = dumped_info
      @old_var = old_var
      @new_var = new_var
      @result = true
    end

    attr_reader :result

    def check_namespace(namespace)
      @dumped_info[namespace.normal].subclass_of?(@owner)
    end

    def rename_class_var?(namespace, node)
      if check_namespace(namespace)
        node.class_vars.each do |id|
          if id.name == @new_var then
            @error_message = "#{namespace.name} already has #{@new_var}\n"
            return false
          end
        end  
      end
      return true
    end

    def visit_method( namespace, node )
      if !rename_class_var?(namespace, node)
        @result = false
      end
    end

    def visit_class_method(namespace, node)
      if !rename_class_var?(namespace, node)
        @result = false
      end
    end

    def visit_class(namespace, node)
      if !rename_class_var?(NodeNamespace.new( node, namespace ), node)
        @result = false
      end
    end
  end

  class ScriptFile

    def get_ancestral_cvar_owner( namespace, dumped_info, var )
      get_owner = GetClassVarOwnerVisitor.new(namespace, dumped_info, var)
      @tree.accept(get_owner)
      get_owner.owner
    end
    
    def rename_class_var( real_owner, dumped_info, old_var, new_var )
      visitor = RenameClassVarVisitor.new(real_owner, dumped_info,
					  old_var, new_var )
      @tree.accept( visitor )
      unless visitor.result.empty? then
	@new_script = RRB.replace_str( @input, visitor.result )
      end
    end

    def rename_class_var?( namespace, dumped_info, old_var, new_var )
     
      visitor = RenameClassVarCheckVisitor.new(namespace, dumped_info,
					       old_var, new_var )
      @tree.accept( visitor )
      @error_message = visitor.error_message unless visitor.result
      return visitor.result
    end

  end

  class Script

    def get_real_cvar_owner( namespace, var )
      @files.inject( namespace ) do |owner,scriptfile|
	scriptfile.get_ancestral_cvar_owner( owner, get_dumped_info, var )
      end
    end
    
    def rename_class_var( namespace, old_var, new_var )
      owner = get_real_cvar_owner( namespace, old_var )
      @files.each do |scriptfile|
	scriptfile.rename_class_var( owner, get_dumped_info,
				    old_var, new_var )
      end
    end

    def rename_class_var?( namespace, old_var, new_var )
      unless RRB.valid_class_var?( new_var )
        @error_message = "#{new_var} is not a valid name for class variables\n"
        return false
      end

      owner = get_real_cvar_owner( namespace, old_var )
      @files.each do |scriptfile|
	if not scriptfile.rename_class_var?( owner,get_dumped_info,
					    old_var, new_var ) then
          @error_message = scriptfile.error_message
	  return false
	end
      end

      return true
      
    end

  end
  
end
