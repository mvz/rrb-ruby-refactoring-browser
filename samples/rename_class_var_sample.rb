
module X
  class A
    @@a = 1
    @@b = 0
    def method_1
      @@a = @@b
    end
  end
  class B < A
    @@c = 0
    def method_1
      @@a = @@c
    end
  end
  class C < A
    @@d = 0
    def method_1
      @@a = @@d
    end
  end
  class D < B
    @@e = 0
    def method_1
      @@a = @@e
    end
  end
end
