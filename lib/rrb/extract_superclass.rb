require 'rrb/script'
require 'rrb/common_visitor'

module RRB

  class ExtractSuperclassVisitor < Visitor
    def initialize( namespace, new_class, targets )
      @new_superclass = '::' + namespace.name + '::' + new_class
      @targets = targets
      @result = []
    end

    attr_reader :result
    
    def visit_class( namespace, node )
      classname = NodeNamespace.new( node, namespace )
      return unless @targets.find{|klass| classname.match?(klass)} 
      return if node.superclass == nil
      @result << Replacer.new_from_id( node.superclass.body, @new_superclass )
    end
  end
  
  class ScriptFile
    
    def extract_superclass( namespace, new_class, targets )
      visitor = ExtractSuperclassVisitor.new( namespace, new_class, targets )
      @tree.accept( visitor )      
      @new_script = RRB.replace_str( @input, visitor.result )
    end

    def reindent_lines_node( lines, node )
      RRB.reindent_lines( lines, node.range.head.head_pointer + INDENT_LEVEL )
    end
    
    def reindent_lines_in( lines, namespace )
      return lines if namespace.match?( Namespace::Toplevel ) 
      reindent_lines_node( lines, namespace.body_node )
    end
    
    def add_superclass_def( lines, lineno )
      indented = reindent_lines_in( lines, class_on( lineno ) ).join
      @new_script = RRB.insert_str( @new_script, lineno, nil, indented, nil )
    end

    def class_on( lineno )
      get_class_on_region( lineno..lineno )
    end
    
  end
  
  class Script

    def superclass_def( namespace, new_class, old_superclass, where )
      result = [
        "class #{new_class} < ::#{old_superclass.name}\n",
        "end\n"
      ]

      ns = namespace
      until ns == where
        result = RRB.reindent_lines( result, INDENT_LEVEL )
        result.unshift( "#{get_dumped_info[ns].type} #{ns.ary[-1]}\n" )
        result.push( "end\n" )
        ns = ns.chop
      end

      result
    end
    
    def extract_superclass( namespace, new_class, targets, path, lineno )
      @files.each do |scriptfile|
        scriptfile.extract_superclass( namespace, new_class, targets )
      end
      
      deffile = @files.find{|scriptfile| scriptfile.path == path}
      new_superclass = get_dumped_info[targets.first].superclass.class_name
      def_str = superclass_def( namespace, new_class,
                                new_superclass,
                                deffile.class_on(lineno).normal )
      deffile.add_superclass_def( def_str, lineno )
    end
    
    def extract_superclass?( namespace, new_class, targets, path, lineno )
      # check namespace exists?
      unless namespace == RRB::Namespace::Toplevel then
        return false if get_dumped_info[namespace] == NullDumpedClassInfo.instance
      end
      # check all targets exist?
      targets.each do |klass|
        return false unless get_dumped_info[klass].type == "class"
      end
      
      # check targets have the same superclass
      superclass = get_dumped_info[targets.first].superclass
      targets.each do |klass|
        return false unless get_dumped_info[klass].superclass == superclass
      end

      # check name collision
      unless get_dumped_info.resolve_const( namespace, new_class ).nil? then
        return false
      end

      # check where new class is defined
      unless namespace.contain?( get_class_on_cursor( path, lineno ).normal )
        return false
      end

      return true
    end
    
  end
end
