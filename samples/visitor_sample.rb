
class TestClassA

  def method_1( a, b )
    c = 3
    call( 3, 2 )
    a.each{|x| print x}
    yield b+c
  end

  def method_2( c = 4, *rest, &block )
    call!
    call
    call = 3
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

  module TestModuleA
    module TestModuleB
      def method_5
      end
    end
  end
  
end

