require 'rrb/parser'

module RRB

  class ScriptFile

    def initialize( input, path )
      @input = input 
      @path = path
      @tree = Parser.new.run( input )
      @new_script = nil
    end
    
    def write_source_to( dir )
      filepath = File.join( dir,@path )
      FileUtils.mkdir_p( File.dirname( filepath ) )
      File.open(  filepath , "w" ) do |file|
        file << @input
      end
    end

    def result_to_io( dst )
      return if @new_script.nil?
      dst << @path
      dst << IO_SPLITTER
      dst << @new_script
      dst << IO_SPLITTER
    end

    def result_rewrite_file
      return if @new_script.nil?
      File.open( @path, "w+" ) do |f|
	f << @new_script
      end
    end
    
    attr_reader :new_script, :path

  end


  Replacer = Struct.new( :lineno, :pointer, :before, :after )
  def Replacer.new_from_id( id, after )
    new( id.lineno, id.pointer, id.name, after )
  end
  
  # guard object 
  Guard = Object.new
  def Guard.lineno
    -1
  end
  
  module_function
  
  def replace_str( input, replace_info )
    src = StringIO.new( input )
    sorted_info = replace_info.sort_by{|i| [ i.lineno, -i.pointer ] }
    sorted_info << Guard
    
    info_index = 0
    dst = ''
    line = src.gets
    
    while line 
      
      if src.lineno == sorted_info[info_index].lineno then
	info = sorted_info[info_index]

	line[ info.pointer-info.before.size, info.before.size ] = info.after
	info_index += 1
      else
	dst << line
	line = src.gets
      end
      
    end

    return dst
  end

  Keywords = [ "__LINE__","__FILE__","BEGIN","END","alias","and","begin","break","case","class","def","defined?","do","else","elsif","end","ensure","false","for","if","in","module","next","nil","not","or","redo","rescue","retry","return","self","super","then","true","undef","unless","until","when","while","yield" ]

  def keyword?( id )
    Keywords.include?( id )
  end
  
  def valid_local_var?( id )
    /^[a-z_][a-zA-Z0-9_]*$/ =~ id && !keyword?( id )
  end

  def valid_instance_var?( id )
    /^@[a-zA-Z0-9_]+$/ =~ id && !keyword?( id )
  end

  def valid_class_var?( id )
    /^@@[a-zA-Z0-9_]+$/ =~ id && !keyword?( id )
  end

  def valid_global_var?( id ) 
    /^\$[a-zA-Z0-9_]+$/ =~ id && !keyword?( id )
  end

  def valid_const?( id )
    /^[A-Z][a-zA-Z0-9_]*$/ =~ id && !keyword?( id )
  end
 
  def valid_method?( id )
    /^[a-z_][a-zA-Z0-9_]*[!?]?$/ =~ id && !keyword?( id )
  end
  
end
