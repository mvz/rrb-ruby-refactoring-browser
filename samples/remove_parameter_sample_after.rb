samples/remove_parameter_sample.rbclass Base
  def base_function(heke)

  end
end

class Derived < Base
  def target_function()
  end

  def using_parameter_function(heke, doga)
    p heke
  end

  def dummy
    a = b = 0
    target_function()
    using_parameter_function(a, b)
    base_function(a)
  end
  
end
-- END --
