
module X
  class A
    def bar
    end
  end
  
  class B < A
    def foo
      puts 1
    end
    def bar
      foo
    end
  end
end
