require 'rrb/script'

module RRB

  
  class RenameMethodAllVisitor < Visitor

    def initialize( old_method, new_method )
      @old_method = old_method
      @new_method = new_method
      @result = []
    end

    attr_reader :result

    def visit_node( namespace, node )      
      node.calls.each do |call|
	if call.name == @old_method then
	  @result << Replacer.new_from_id( call, @new_method )
	end
      end
    end
    
    def visit_method( namespace, method_node )
      
      if method_node.name == @old_method then
	@result << Replacer.new_from_id( method_node.name_id, @new_method )
      end
    end

    def visit_singleton_method( namespace, s_method_node )
      visit_method( namespace, s_method_node )
    end

    def visit_class_method( namespace, c_method_node )
      visit_method( namespace, c_method_node )
    end
    
  end

  class RenameMethodAllCheckVisitor < Visitor
    
    def initialize( old_method, new_method )
      @old_method = old_method
      @new_method = new_method
      @result = true
    end

    def visit_node( namespace, node )
      if node.fcalls.find{|fcall| fcall.name == @old_method } &&
	  node.local_vars.find{|var| var.name == @new_method } then
	@result = false
      end
    end

    attr_reader :result
  end

  class MethodDefineCheckVisitor < Visitor

    def initialize( method, classes )
      @method = method
      @classes = classes
    end

    def visit_class( namespace, node )
      if node.method_defs.inject( false ){|r,i| (r || i.name == @method) } then
	classname = NodeNamespace.new( node, namespace ).str
	@classes.delete_if{|class_info| class_info.class_name ==  classname }
      end
    end

    def visit_toplevel( namespace, node )
      if node.method_defs.inject( false ){|r,i| r || i.name == @method } then
	@classes.delete_if{|class_info| class_info.class_name ==  'Object' }
      end
    end
    
  end

  class ScriptFile

    def rename_method_all( old_method, new_method )
      visitor = RenameMethodAllVisitor.new( old_method, new_method )
      @tree.accept( visitor )
      unless visitor.result.empty?
	@new_script = RRB.replace_str( @input, visitor.result )
      end
    end

    def rename_method_all?( old_method, new_method )
      return false unless RRB.valid_method?( new_method )
      visitor = RenameMethodAllCheckVisitor.new( old_method, new_method )
      @tree.accept( visitor )
      return visitor.result
    end

    def method_define_check( method, classes )
      @tree.accept MethodDefineCheckVisitor.new( method, classes )
    end

  end

  class Script
    
    def rename_method_all( old_method, new_method )
      @files.each do |scriptfile|
	scriptfile.rename_method_all( old_method, new_method )
      end
    end
    
    def rename_method_all?( old_method, new_method )
      info = get_dumped_info
      
      info.each do |class_info|
	has_old_method = class_info.has_method?( old_method ) 
	has_new_method = class_info.has_method?( new_method ) 
	return false if has_old_method && has_new_method
      end

      refactored_classes = info.classes_having_method( old_method )      
      @files.each do |scriptfile|
	scriptfile.method_define_check( old_method, refactored_classes )
      end
      return false unless refactored_classes.empty?
      
      @files.each do |scriptfile|
	unless scriptfile.rename_method_all?( old_method, new_method ) then
	  return false
	end
      end
      
      return true
    end

  end
  
end
  
