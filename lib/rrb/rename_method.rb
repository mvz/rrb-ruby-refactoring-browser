
module RRB

  class ClassName

    def initialize( str )
      @nest = str.split(/::/)
    end

    def match?( namespace )
      namespace.match?( @nest )
    end
    
  end
  
  class RenameMethodVisitor < Visitor

    def initialize( classes, old_method, new_method )
      @classes = classes
      @old_method = old_method
      @new_method = new_method
      @result = []
    end

    attr_reader :result
    
    def visit_method( namespace, node )
      
      @classes.each do |classname|
	if classname.match?( namespace ) &&  @old_method == node.name then
	  @result << Replacer.new( node.name_id.lineno,
				  node.name_id.pointer,
				  node.name_id.name,
				  @new_method )	  
	end
	if classname.match?( namespace ) then
	  node.fcalls.each do |fcall|
	    if fcall.name == @old_method then
	      @result << Replacer.new( fcall.lineno,
				      fcall.pointer,
				      fcall.name,
				      @new_method )
	    end
	  end
	end
	
      end

    end

  end
  
  
  class Script

    def rename_method
    end

    def rename_method?
    end
    
  end

  class ScriptFile
    
    def rename_method( class_pathes, old_name, new_name )

      visitor = RenameMethodVisitor.new( class_pathes, old_name, new_name )
      @tree.accept( visitor )
      @new_script = RRB.replace_str( @input, visitor.result )
    end

    def rename_method?
    end

  end
  
end
