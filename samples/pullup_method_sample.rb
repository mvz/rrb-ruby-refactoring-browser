class Base
  def hoge
    p 1
  end
end

class Derived < Base
  def foo
    bar
  end
  def bar
    hoge
    p 2
  end
end