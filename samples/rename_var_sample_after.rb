samples/rename_var_sample.rb
# comment

class Rename
  def method_1( x, y )
    bb = 3
    bb.upto(6) do |i|
      print i*3, "\n"
    end
    print bb**4, bb**5, "#{bb}", " #{3*bb**2}"
  end
end
-- END --
