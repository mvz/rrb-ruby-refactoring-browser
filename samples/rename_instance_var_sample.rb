
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
end
