require 'rrb/script'
require 'set'
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
        @error_message = "#{@new_method} is already used as a local variable at #{NodeNamespace.new(node, namespace).name}\n"
	@result = false
      end
    end

    attr_reader :result
  end

  class ClassesDefineMethodVisitor < Visitor

    def initialize( method )
      @method = method
      @classes = Set.new
    end

    attr_reader :classes
    
    def visit_method( namespace, node )
      if node.name == @method then
        ns = namespace.normal
        if ns == Namespace::Toplevel then
          @classes.add Namespace::Object
        else
          @classes.add ns
        end
      end
    end
    
  end
  
  class ScriptFile

    def classes_define_method( method )
      visitor = ClassesDefineMethodVisitor.new( method )
      @tree.accept( visitor )
      visitor.classes
    end
    
    def rename_method_all( old_method, new_method )
      visitor = RenameMethodAllVisitor.new( old_method, new_method )
      @tree.accept( visitor )
      unless visitor.result.empty?
	@new_script = RRB.replace_str( @input, visitor.result )
      end
    end

    def rename_method_all?( old_method, new_method )
      visitor = RenameMethodAllCheckVisitor.new( old_method, new_method )
      @tree.accept( visitor )
      @error_message = visitor.error_message unless visitor.result
      return visitor.result
    end

  end

  class Script
    def rename_method_all( old_method, new_method )
      @files.each do |scriptfile|
	scriptfile.rename_method_all( old_method, new_method )
      end
    end

    def classes_define_method( method )
      classes = Set.new
      @files.each do |scriptfile|
        classes.merge scriptfile.classes_define_method( method )
      end
      classes
    end
    
    def rename_method_all?( old_method, new_method )
      unless RRB.valid_method?( new_method )
        @error_message = "#{new_method} is not a valid name for methods\n"
        return false
      end

      info = get_dumped_info
      
      info.each do |class_info|
	has_old_method = class_info.has_method?( old_method ) 
	has_new_method = class_info.has_method?( new_method ) 
	if has_old_method && has_new_method
          @error_message = "#{class_info.class_name.name} already has #{new_method}\n"
          return false
        end
      end

      refactored_classes = info.classes_having_method( old_method )
      refactored_classes.map!{|c| c.class_name}
      lhs = Set.new(refactored_classes)
      rhs = classes_define_method(old_method)
      diff = (lhs | rhs) - (lhs & rhs)

      if diff.include?(Namespace.new("Kernel"))
        @error_message = "Don't rename kernel method\n"
        return false
      end
      
      @files.each do |scriptfile|
	unless scriptfile.rename_method_all?( old_method, new_method ) then
          @error_message = scriptfile.error_message
	  return false
	end
      end
      
      return true
    end

  end
  
end
  
