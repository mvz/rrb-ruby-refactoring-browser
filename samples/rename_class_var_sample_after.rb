samples/rename_class_var_sample.rb
module X
  class A
    @@c = 3
    def method_1
      @@c = 0
    end
  end
  class B < A
    @@b = 2
    def method_1
      @@b = @@c
    end
  end
  class C
    @@a = 0
  end
end
module Y
  class A
    @@a = 2
  end
end
-- END --
