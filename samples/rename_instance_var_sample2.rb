
require 'samples/rename_instance_var_sample'
module X
  class C < A
    def method_1
      @a = 0
      @d = 0
    end
  end
  class D < B
    def method_1
      @a = 0
      @e = 0
    end
  end
end
