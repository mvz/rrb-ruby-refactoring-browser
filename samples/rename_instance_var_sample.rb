
module X
  class A
    def method_1
      @a = 0
      @b = 0
    end
  end
  class B < A
    def method_1
      @a = 0
      @c = 0
    end
  end
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
