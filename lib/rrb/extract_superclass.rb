require 'rrb/script'

module RRB

  class ExtractSuperclassVisitor < Visitor
    def initialize( namespace, new_class, targets )
      @new_superclass = '::' + namespace.str + '::' + new_class
      @targets = targets
      @result = []
    end

    attr_reader :result
    
    def visit_class( namespace, node )
      classname = NodeNamespace.new( node, namespace )
      return unless @targets.find{|klass| classname.match?(klass)} 
      return if node.superclass == nil
      @result << Replacer.new( node.superclass.body.lineno,
                               node.superclass.body.pointer,
                               node.superclass.name,
                               @new_superclass )
    end
  end
  
  class ScriptFile
    
    def extract_superclass( namespace, new_class, targets )
      visitor = ExtractSuperclassVisitor.new( namespace, new_class, targets )
      @tree.accept( visitor )      
      unless visitor.result.empty?
        @new_script = RRB.replace_str( @input, visitor.result )
      end
    end

    def add_new_superclass( namespace, new_class, old_superclass, dumped_info )
      @new_script << new_superclass_def(namespace, new_class, old_superclass, dumped_info ).join("\n")
    end

    def new_superclass_def( namespace, new_class, old_superclass, dumped_info )
      result = [ "class #{new_class} < ::#{old_superclass.class_name}", "end" ]

      ns = namespace
      until ns == Namespace::Toplevel
        result = indent_lines( result )
        result.unshift( "#{dumped_info[ns].type} #{ns.ary[-1]}" )
        result.push( "end" )
        ns = ns.chop
      end

      result.push("")
      result
    end
    
    def indent_lines( lines, lv = 1 )
      lines.map{|line| " " * ( INDENT_LEVEL * lv ) + line }
    end
  end
  
  class Script

    def extract_superclass( namespace, new_class, targets, path )
      @files.each do |scriptfile|
        scriptfile.extract_superclass( namespace, new_class, targets )
      end
      
      deffile = @files.find{|scriptfile| scriptfile.path == path}
      deffile.add_new_superclass( namespace, new_class,
                                  get_dumped_info[targets.first].superclass,
                                  get_dumped_info )
    end
    
    def extract_superclass?( namespace, new_class, targets )
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
      ns = namespace
      until ns == RRB::Namespace::Toplevel
        return false if get_dumped_info[ns].consts.include?( new_class )
        ns = ns.chop
      end
      get_dumped_info[namespace].ancestors.each do |anc|
        return false if anc.consts.include?( new_class )
      end
      
      return true
    end
    
  end
end
