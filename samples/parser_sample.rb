
class TestClassA

  def <=>( a, b )
  end

  def method_1( a, b )
    c = 3; @x = 2
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

  class << TestClassB
    def method_7( z )
      z ** 3
    end
  end

  def method_8
    $x = 5
    @y = $x + 4
    return "str #{$xx*@y*@@z}"
  end

  def method_9
    p Const1
    print Const2::Const1, "\n"
    for i in ::Const2::Const1
      puts self::Const3
      puts ConstCall1()
      puts Const4::Const5::ConstCall2()
      puts self.ConstCall3
    end
    p Const6::Const7::Const8::Const9
    heke = "str #{::Const10::Const11 ** Const12}"
  end

  Const6 = 4
end

class TestClassD < TestClassA::TestClassB
end
