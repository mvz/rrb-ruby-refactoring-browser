
module X
  class A
    @@a = 3
    def method_1
      @@a = 0
    end
  end
  class B < A
    @@b = 2
    def method_1
      @@b = @@a
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
