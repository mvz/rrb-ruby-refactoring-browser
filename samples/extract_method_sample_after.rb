
class A
  def bar(x, y)
    z = 2 * x
    w = 2 * y
    puts "#{x},#{y}"
    puts "#{z},#{w}"
  end
  def foo
    x = 1
    y = 2
    bar(x, y)
  end
  def foo2
    x = 1
    y = 2
  end
end
