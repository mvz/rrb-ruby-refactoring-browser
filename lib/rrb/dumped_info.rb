
module RRB

  class DumpedInfo

    def initialize( hash )
      @classes = hash
    end

    def [](index)
      @classes[index]
    end
    
    def DumpedInfo.get_dumped_info( io )
      info_hash = {}
      while line = io.gets
	split_list = line.chomp.split( /#/, -1 )
	info = DumpedClassInfo.new( split_list[0],
			      split_list[1].split(/;/),
			      split_list[2].split(/;/),
			      split_list[3].split(/;/),
			      split_list[4].split(/;/),
			      split_list[5].split(/;/) )
	info_hash[info.class_name] = info
      end

	  
      new(info_hash)
    end
  end
  
  class DumpedClassInfo
    
    
    def initialize( type, ancestor_names, public_method_names,
		   protected_method_names, private_method_names,
		   singleton_method_names )
      @type = type
      @class_name = ancestor_names[0]
      @ancestor_names = ancestor_names[1..-1]
      @public_method_names = public_method_names
      @protected_method_names = protected_method_names
      @private_method_names = private_method_names
      @singleton_method_names = singleton_method_names
    end
    
    attr_reader( :type, :class_name, :ancestor_names, :public_method_names,
		:protected_method_names, :private_method_names,
		:singleton_method_names )
    
    def have_method?( methodname )
      return true if @public_method_names.include?( methodname )
      return true if @protected_method_names.include?( methodname )
      return true if @private_method_names.include?( methodname )
      return false
    end
    
  end

end
