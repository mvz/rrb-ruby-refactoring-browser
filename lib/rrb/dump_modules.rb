
module RRB
  DUMP_MODULES_SCRIPT = <<'EOS'
ObjectSpace.each_object( Module ) do |mod|

  # 0 class type
  case mod
  when Class
    print "class"
  when Module
    print "module"
  else
    print "unknown"
  end
  
  print "#"

  # 1 class hierarchy( first of this is name of this class )
  mod.ancestors.each do |ancestor|
    print ancestor.name, ";"
  end

  print "#"

  # 2 public instance methods
  mod.public_instance_methods(false).each do |method_name|
    print method_name, ";"
  end

  print "#"

  # 3 protected instance methods
  mod.protected_instance_methods(false).each do |method_name|
    print method_name, ";"
  end

  print "#"

  # 4 private instance methods
  mod.private_instance_methods(false).each do |method_name|
    print method_name, ";"
  end

  print "#"

  # 5 singleton_methods
  class << mod
    public_instance_methods(false).each do |method_name|
      print method_name, ";"
    end
  end

  print "\n"
  
end

EOS

end
