samples/rename_class_var_sample.rb
module X
  class A
    @@f = 1
    @@b = 0
    def method_1
      @@f = @@b
    end
    def A.method_2
      @@f = 2
    end
  end
  class B < A
    @@c = 0
    def method_1
      @@f = @@c
    end
  end
  class C < A
    @@d = 0
    def method_1
      @@f = @@d
    end
  end
  class D < B
    @@e = 0
    def method_1
      @@f = @@e
    end
  end
end
-- END --
