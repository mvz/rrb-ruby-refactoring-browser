class A
  def a
  end
end

class B < A
  def x
    p 1
    a
  end
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
  def w
  end
end
