
module RRB
  
  class DumpedInfo
    
    def DumpedInfo.get_dumped_info( io )
      info_hash = {}
      while line = io.gets
	split_list = line.chomp.split( /#/, -1 )
	info = DumpedInfo.new( split_list[0],
			      split_list[1].split(/;/),
			      split_list[2].split(/;/),
			      split_list[3].split(/;/),
			      split_list[4].split(/;/),
			      split_list[5].split(/;/) )
	info_hash[info.module_name] = info
      end
      
      info_hash
    end
    
    def initialize( type, ancestor_names, public_method_names,
		   protected_method_names, private_method_names,
		   singleton_method_names )
      @type = type
      @module_name = ancestor_names[0]
      @ancestor_names = ancestor_names[1..-1]
      @public_method_names = public_method_names
      @protected_method_names = protected_method_names
      @private_method_names = private_method_names
      @singleton_method_names = singleton_method_names
    end
    
    attr_reader( :type, :module_name, :ancestor_names, :public_method_names,
		:protected_method_names, :private_method_names,
		:singleton_method_names )
    
  end

end
