samples/pullup_method_sample.rbclass Base
  def bar
  end
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
  end
end
-- END --
