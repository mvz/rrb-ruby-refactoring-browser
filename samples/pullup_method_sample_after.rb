samples/pullup_method_sample.rbclass Base
  def bar
    hoge
    p 2
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
  def asdf
  end
end
-- END --
