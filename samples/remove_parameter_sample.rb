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
    target_function(0)
    using_parameter_function(0)
    base_function(0)
  end
  
end
