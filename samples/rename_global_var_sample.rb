$x = 3
$z = $x

class Rename
  def method_1(x, y)
    @x = $x
    @@x = $x
    $x = 2

    $x.upto(6) do |x|
      print x*3, "\n"
    end
  end
end
