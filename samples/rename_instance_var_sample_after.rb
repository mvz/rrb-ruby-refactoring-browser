samples/rename_instance_var_sample.rb
module X
  class A
    @d = 3
    def method_1
      @d = 0
    end
    def self.method_2
      @b = @d
    end
  end
  class B < A
    @d = 3
    @c = 3
    def method_1
      @d = 0
    end
  end
  class C
    @a = 3
    def method_1
      @a = 0
    end
  end
end
module Y
  class A
    @a = 3
  end
end
-- END --
