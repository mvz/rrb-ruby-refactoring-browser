
class A

  def initialize
    @i = 0
  end
  
  def foo( x )
    @i += x
  end

  def baz
    @i
  end
  
end

class B < A

  def foo( x )
    @i += x*2
  end

  def bar
    hek = 5
    foo 1 
    baz
  end

  def B.foo
    obj = Object.new
    
    def obj.foo
      return "foo"
    end

    obj.foo
  end
  
end

if __FILE__ == $0 then
  obj = B.new
  
  obj.foo
  p obj.bar
  class << obj
    def foo
      bar
    end
  end

  p obj.foo
end
