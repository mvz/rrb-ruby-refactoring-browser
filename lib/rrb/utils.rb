
module Enumerable

  if RUBY_VERSION < '1.7' then
    
    def sort_by
      ary = map { |i| [yield(i), i] }
      ary.sort! { |a, b| a[0] <=> b[0] }
      ary.map! { |i| i[1] }
    end

    def inject(*argv)
      argc = argv.size
      
      if argc == 0
	first = true
	result = nil

	each { |e|
	  if first
	    first = false
	    result = e
	  else
	    result = yield(result, e)
	  end
	}
      elsif argc == 1
	result = argv[0]
	
	each { |e| result = yield(result, e) }
      else
	raise ArgumentError, "wrong # of arguments(#{argc} for 1)"
      end
      
      result
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
