require 'rrb/script'
require 'rrb/common_visitor'


module RRB
  
  class Script

    def get_class_on_cursor(path, lineno)
      get_class_on_region(path, lineno..lineno)
    end

    def get_method_on_cursor(path, lineno)
      get_method_on_region(path, lineno..lineno)
    end
  end
end
