require 'rrb/script'

module RRB

  class GetClassVarOwnerVisitor < Visitor
    def initialize(namespace, dumped_info, old_var)
      @str_namespace = namespace.join('::')
      @old_var = old_var
      @dumped_info = dumped_info
      @my_info = dumped_info[@str_namespace]
      @owner = @str_namespace
    end

    attr_reader :owner
   
    def visit_class(namespace, node)
      str_namespace = namespace.map{|i| i.name}.join('::')
      if str_namespace.empty?
        str_namespace = node.name
      else
        str_namespace = str_namespace + '::' + node.name
      end
      
      return false unless node.class_vars.map{|i| i.name}.include?(@old_var)
      ancestor_names = @dumped_info[@owner].ancestor_names
      index = ancestor_names.index(str_namespace)
      if index
        @owner = ancestor_names[index]
      end
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
    
    def check_namespace(str_namespace)
      info = @dumped_info[str_namespace]
      return false unless info
      return false unless info.ancestor_names.include?(@owner) || str_namespace == @owner
      return true
    end

    def rename_class_var(str_namespace, node)
      if check_namespace(str_namespace)
        node.class_vars.each do |id|
          if id.name == @old_var then
            @result <<
                     Replacer.new( id.lineno, id.pointer, @old_var, @new_var )
          end
        end  
      end
    end

    def visit_method( namespace, node )
      str_namespace = namespace.map{|i| i.name}.join('::')
      rename_class_var(str_namespace, node)
    end

    def visit_class_method(namespace, node)
      str_namespace = namespace.map{|i| i.name}.join('::')
      rename_class_var(str_namespace, node)
    end

    def visit_class(namespace, node)
      str_namespace = namespace.map{|i| i.name}.join('::')
      if namespace.empty?
        str_namespace = node.name
      else
        str_namespace = str_namespace + '::' + node.name
      end
      rename_class_var(str_namespace, node)
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

    def check_namespace(str_namespace)
      info = @dumped_info[str_namespace]
      return false unless info
      return false unless info.ancestor_names.include?(@owner) || str_namespace == @owner
      return true
    end

    def rename_class_var?(str_namespace, node)
      if check_namespace(str_namespace)
        node.class_vars.each do |id|
          if id.name == @new_var then
            return false
          end
        end  
      end
      return true
    end

    def visit_method( namespace, node )
      str_namespace = namespace.map{|i| i.name}.join('::')
      if !rename_class_var?(str_namespace, node)
        @result = false
      end
    end

    def visit_class_method(namespace, node)
      str_namespace = namespace.map{|i| i.name}.join('::')
      if !rename_class_var?(str_namespace, node)
        @result = false
      end
    end

    def visit_class(namespace, node)
      str_namespace = namespace.map{|i| i.name}.join('::')
      if namespace.empty?
        str_namespace = node.name
      else
        str_namespace = str_namespace + '::' + node.name
      end
      if !rename_class_var?(str_namespace, node)
        @result = false
      end
    end
  end

  class ScriptFile
    
    def rename_class_var( namespace, dumped_info, old_var, new_var )
      get_owner = GetClassVarOwnerVisitor.new(namespace, dumped_info, old_var)
      @tree.accept(get_owner)
      visitor = RenameClassVarVisitor.new(get_owner.owner, dumped_info,
					  old_var, new_var )
      @tree.accept( visitor )
      @new_script = RRB.replace_str( @input, visitor.result )
    end

    def rename_class_var?( namespace, dumped_info, old_var, new_var )
      return false unless RRB.valid_class_var?( new_var )
      get_owner = GetClassVarOwnerVisitor.new(namespace, dumped_info, old_var)
      @tree.accept(get_owner)
      visitor = RenameClassVarCheckVisitor.new(get_owner.owner, dumped_info,
					       old_var, new_var )
      @tree.accept( visitor )
      return visitor.result
    end

  end

  class Script
    
    def rename_class_var( namespace, old_var, new_var )
      @files.each do |scriptfile|
	scriptfile.rename_class_var( namespace, get_dumped_info,
				    old_var, new_var )
      end
    end

    def rename_class_var?( namespace, old_var, new_var )
      @files.each do |scriptfile|
	if not scriptfile.rename_class_var?( namespace,get_dumped_info,
					    old_var, new_var ) then
	  return false
	end
      end

      return true
      
    end

  end
  
end
