
module X
  class A
    def foo
      puts 1
    end
    def bar
    end
  end
  
  class B < A
    def bar
      foo
    end
  end
end
