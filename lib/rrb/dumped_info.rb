
module RRB

  class DumpedInfo

    def initialize( hash )
      @classes = hash
    end

    def [](index)
      @classes[index]
    end

    def each( &block )
      @classes.each_value( &block )
    end

    def classes_having_method( method )
      result = []
      @classes.each_value do |info|
	if info.has_method?( method, false ) then
	  result << info
	end
      end

      result
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
				   split_list[5].split(/;/),
				   split_list[6].split(/;/) )
	info_hash[info.class_name] = info
      end
      
      info_hash.each_value do |info|
	info.ancestors = info.ancestor_names.map{|name| info_hash[name]}
      end
      new(info_hash)
    end
  end
  
  class DumpedClassInfo
    
    
    def initialize( type, ancestor_names, public_method_names,
		   protected_method_names, private_method_names,
		   singleton_method_names, consts )
      @type = type
      @class_name = ancestor_names[0]
      @ancestor_names = ancestor_names[1..-1]
      @public_method_names = public_method_names
      @protected_method_names = protected_method_names
      @private_method_names = private_method_names
      @singleton_method_names = singleton_method_names
      @consts = consts
    end
    
    attr_reader( :type, :class_name, :ancestor_names, :public_method_names,
		:protected_method_names, :private_method_names,
		:singleton_method_names, :consts )

    attr_accessor :ancestors
    
    def has_method?( methodname, inherited_too=true )
      if inherited_too then
	return true if has_method?( methodname, false )
	@ancestors.each do |ancestor|
	  return true if ancestor.has_method?( methodname, false )
	end
	return false
      end
      
      return true if @public_method_names.include?( methodname )
      return true if @protected_method_names.include?( methodname )
      return true if @private_method_names.include?( methodname )
      return false
    end

    def subclass_of?(classname)
      @ancestor_names.include?(classname) ||  @class_name == classname
    end
  end

end
