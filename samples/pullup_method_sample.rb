class Base

  def hoge
    p 1
  end
  def asdf
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
  def asdf
  end
  class Derived2 < Base
    def bar
    end
  end
end
