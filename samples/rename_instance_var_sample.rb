
module X
  class A
    @@a = 3
    def method_1
      @a = 0
      @a = @b
    end
  end
  class B < A
    def method_1
      @a = 0
      @c = 1
    end
  end
  class C
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
