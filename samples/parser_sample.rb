
class TestClassA

  def <=>( a, b )
  end

  def method_1( a, b )
    c = 3
    call( 3, 2 )
    a.each{|x| print x}
    yield b+c
  end

  def method_2( c = 4, *rest, &block )
    call!
    call
    call = nil
    call
    p Const_Accsess
  end

  def method_3( x )
    ConstNameCall( 2 )    
  end
  
  
  class TestClassB

    def method_4
    end

    class TestClassC
    end
    
  end
  
  def TestClassA.method_5( x, y )
    x + y
  end

  def TestClassB.method_6
  end
  
end

