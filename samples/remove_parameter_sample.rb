class Base
  def base_function(heke)

  end
end

class Derived < Base
  def target_function(heke)
  end

  def using_parameter_function(heke, doga)
    p heke
  end

  def dummy
    a = b = 0
    target_function(a)
    using_parameter_function(a, b)
    base_function(a)
  end
  
end
