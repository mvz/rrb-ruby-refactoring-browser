samples/rename_method_sample.rb
class A

  def initialize
    @i = 0
  end
  
  def feefoo( x )
    @i += x
  end

  def baz
    @i
  end
  
end

class B < A

  def feefoo( x )
    @i += x*2
  end

  def bar
    hek = 5
    feefoo 1 
    baz
  end

  def B.feefoo
    obj = Object.new
    
    def obj.feefoo
      return "foo"
    end

    obj.feefoo
  end
  
end

if __FILE__ == $0 then
  obj = B.new
  
  obj.feefoo
  p obj.bar
  class << obj
    def feefoo
      bar
    end
  end

  p obj.feefoo
end
-- END --
