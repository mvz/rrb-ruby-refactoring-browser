require 'rrb/script'
require 'rrb/common_visitor'


module RRB
  def f
  end
  class ScriptFile

    def get_method_on_cursor(lineno)
      visitor = GetMethodOnRegionVisitor.new(lineno, lineno)
      @tree.accept( visitor )
      visitor.namespace
    end

    def get_class_on_cursor(lineno)
      visitor = GetClassOnRegionVisitor.new(lineno, lineno)
      @tree.accept( visitor )
      visitor.namespace
    end    
  end
  
  class Script

    def get_class_on_cursor(path, lineno)
      @files.each do |scriptfile|
        if scriptfile.path == path 
          return scriptfile.get_class_on_cursor(lineno)
        end
      end
    end

    def get_method_on_cursor(path, lineno)
      @files.each do |scriptfile|
        if scriptfile.path == path
          return scriptfile.get_method_on_cursor(lineno)
        end
      end
    end
  end
end
