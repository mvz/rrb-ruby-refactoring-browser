
module Enumerable

  if RUBY_VERSION < '1.7' then
    
    def sort_by
      ary = map { |i| [yield(i), i] }
      ary.sort! { |a, b| a[0] <=> b[0] }
      ary.map! { |i| i[1] }
    end
    
  end
  
end

class IO

  if defined?(:read) then
    def IO.read( path )
      open( path ) do |f|
	f.read
      end
    end
  end
end

module RRB
  
  class RRBError < StandardError
  end

end
