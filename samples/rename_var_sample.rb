
# comment

class Rename
  def method_1( x, y )
    z = 3
    z.upto(6) do |i|
      print i*3, "\n"
    end
    print z**4, z**5, "#{z}", " #{3*z**2}"
  end
end
