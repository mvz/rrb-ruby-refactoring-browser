require 'rrb/scriptfile.rb'
require 'stringio'

module RRB

  STREAM_SPLITTER = "\C-l"
  
  class Script

    def initialize( files )
      @files = files
    end

    def rename_local_var( namespace, method_name, old_var, new_var )
      @files.each do |scriptfile|
	scriptfile.rename_local_var( namespace, method_name,
				    old_var, new_var )
      end
    end

    def rename_local_var?( namespace, method_name, old_var, new_var )
      @files.each do |scriptfile|
	if not scriptfile.rename_local_var?( namespace, method_name,
					    old_var, new_var ) then
	  return false
	end
      end

      return true
      
    end

    def result_to_io( dst )

      @files.each do |scriptfile|
	dst << scriptfile.name
	dst << STREAM_SPLITTER
	dst << scriptfile.new_script
	dst << STREAM_SPLITTER
      end

      dst << '-- END --'
      dst << STREAM_SPLITTER
      dst << "\n"
    end

    def result_overwrite_file
      @files.each do |scriptfile|
	File.open( scriptfile.name, "w+" ) do |f|
	  f << scriptfile.new_script
	end
      end
    end

    
    def Script.new_from_io( input )

      files = []
      loop do	
	name = input.gets( STREAM_SPLITTER ).chop
	break if name == "-- END --"
	content = input.gets( STREAM_SPLITTER ).chop
	files << ScriptFile.new( StringIO.new( content ), name )
      end

      return new( files )
    end

    def Script.new_from_filenames( filenames )
      files = []
      filenames.each do |filename|
	files << ScriptFile.new( File.open(filename), filename )
      end
      return new( files )
    end
    
  end
  
end
