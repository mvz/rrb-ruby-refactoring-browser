samples/rename_instance_var_sample.rb
module X
  class A
    def method_1
      @f = 0
      @b = 0
    end
  end
  class B < A
    def method_1
      @f = 0
      @c = 0
    end
  end
end
samples/rename_instance_var_sample2.rb
require 'samples/rename_instance_var_sample'
module X
  class C < A
    def method_1
      @f = 0
      @d = 0
    end
  end
  class D < B
    def method_1
      @f = 0
      @e = 0
    end
  end
end
-- END --
