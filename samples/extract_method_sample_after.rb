

class C
  def hogehoge
  end
end
class B < C
  def bar(x, y)
    z = 2 * x
    z = 3 * z
    w = 2 * y
    puts "#{x},#{y}"
    return z, w
  end
  def foo
    x = 1
    y = 2
    z, w = bar(x, y)
    puts "#{z},#{w}"
  end
end
def bar
end

x = 1
y = 2
