
class Array

  def top
    self[-1]
  end

end

module Enumerable

  if RUBY_VERSION < '1.7' then
    
    def sort_by
      ary = map { |i| [yield(i), i] }
      ary.sort! { |a, b| a[0] <=> b[0] }
      ary.map! { |i| i[1] }
    end
    
  end
  
end

module RRB
  
  class RRBError < StandardError
  end

end
