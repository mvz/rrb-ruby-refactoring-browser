require 'rrb/scriptfile'
require 'rrb/script'
require 'rrb/node.rb'
require 'rrb/parser.rb'

require 'set'

module RRB
  
  class RenameMethodVisitor < Visitor

    def initialize( classes, old_method, new_method )
      @classes = classes
      @old_method = old_method
      @new_method = new_method
      @result = []
    end

    attr_reader :result

    def warning_piece( namespace, num_spaces )
      
      "def #{@old_method}(*arg); \
raise '#{namespace.str}##{@old_method} is renamed #{@new_method}' end\n" +
	" "*num_spaces
    end

    def rename_method_def( namespace, id, head_keyword )
      @result << Replacer.new_from_id( id, @new_method )
      @result << Replacer.new( id.lineno, head_keyword.head_pointer,
			      "",
			      warning_piece( namespace,
					    head_keyword.head_pointer ) )
    end

    def rename_fcalls( fcalls )
      fcalls.each do |fcall|
	if fcall.name == @old_method then
	  @result << Replacer.new_from_id( fcall, @new_method )
	end
      end
    end
    
    def visit_method( namespace, node )
      
      @classes.each do |classname|
	if namespace.match?( classname ) &&  @old_method == node.name then
	  rename_method_def( namespace, node.name_id, node.head_keyword )
	end
	if namespace.match?( classname ) then
	  rename_fcalls( node.fcalls )
	end
	
      end

    end

  end
    
  class GetAllClassesCallMethod < Visitor

    def initialize( methodname )
      @method = methodname
      @classes = []
    end

    attr_reader :classes
    
    def visit_method( namespace, node )
      if node.fcalls.find{|fcall| fcall.name == @method } then
	@classes << namespace.str
      end
    end

  end

  class Script

    def classes_call_method( methodname )
      classes = Set.new
      @files.each do |scriptfile|
	classes.merge scriptfile.classes_call_method( methodname )
      end
      classes
    end
    
    def classes_access_method( basis, methodname )
      classes = []
      result = []
      basis.each do |namespace|
	classes << namespace.str
	get_dumped_info[namespace.str].ancestors.each do |ancestor|
	  if ancestor.has_method?( methodname )
	    classes << ancestor.class_name
	  end
	end
      end
      classes_call_method( methodname ).each do |classname|
	basis.each do |namespace|
	  if get_dumped_info[namespace.str].subclass_of?(classname)
	    classes << classname
	  end
	end
      end
      

      get_dumped_info.each do |classinfo|
	classes.each do |classname|
	  if classinfo.subclass_of?(classname) then
	    result << Namespace.new( classinfo.class_name )
	    break
	  end
	end
      end

      result
    end
    
    def rename_method( base_classes, old_method, new_method )
      real_classes = classes_access_method( base_classes, old_method )
      @files.each do |scriptfile|
	scriptfile.rename_method( real_classes, old_method, new_method )
      end
    end

    def rename_method?( base_classes, old_method, new_method )
      return false unless RRB.valid_method?( new_method )
      classes_access_method( base_classes, old_method ).each do |ns|
	if get_dumped_info[ns.str].has_method?( new_method ) then
	  return false
	end
      end
      true
    end
    
  end

  class ScriptFile

    def classes_call_method( methodname )
      visitor = GetAllClassesCallMethod.new( methodname )
      @tree.accept( visitor )
      visitor.classes
    end
    
    def rename_method( class_pathes, old_method, new_method )
      visitor = RenameMethodVisitor.new( class_pathes, old_method, new_method )
      @tree.accept( visitor )
      @new_script = RRB.replace_str( @input, visitor.result )
    end


  end
  
end
