samples/pushdown_method_sample.rbclass A
  def a
  end
end

class B < A
  def y
    z
  end
  def z
  end
  def w
  end

  private :z
end

class C < B
  def x
    p 1
    a
  end
  def w
  end
  class D < B
  end
end
-- END --
