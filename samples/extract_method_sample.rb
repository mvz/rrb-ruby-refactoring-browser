
class A
  class C
    def hogehoge
    end
  end
  class B
    def foo
      x = 1
      y = 2
      z = 2 * x
      z = 3 * z
      w = 2 * y
      puts "#{x},#{y}"
      puts "#{z},#{w}"
    end
  end
  def bar
  end
end
x = 1
y = 2
