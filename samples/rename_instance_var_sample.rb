
module X
  class A
    @a = 3
    def method_1
      @a = 0
    end
    def self.method_2
      @b = @a
    end
  end
  class B < A
    @a = 3
    @c = 3
    def method_1
      @a = 0
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
