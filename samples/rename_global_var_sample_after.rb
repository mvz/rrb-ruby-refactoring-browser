$y = 3
$z = $y

class Rename
  def method_1(x, y)
    @x = $y
    @@x = $y
    $y = 2

    $y.upto(6) do |x|
      print x*3, "\n"
    end
  end
end
