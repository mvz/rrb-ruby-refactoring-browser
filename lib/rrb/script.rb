require 'rrb/scriptfile.rb'
require 'stringio'
require 'fileutils'
require 'find'
require 'rrb/dumped_info.rb'
require 'rrb/default'

module RRB

  IO_SPLITTER = "\C-l"
  IO_TERMINATOR = '-- END --'
  
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

    def rename_method_all?( old_method, new_method )      
      @files.each do |scriptfile|
	unless scriptfile.rename_method_all?( old_method, new_method ) then
	  return false
	end
      end
      return true
    end
    
    def result_to_io( dst )

      @files.each do |scriptfile|
	dst << scriptfile.path
	dst << IO_SPLITTER
	dst << scriptfile.new_script
	dst << IO_SPLITTER
      end

      dst << IO_TERMINATOR
      dst << IO_SPLITTER
      dst << "\n"
    end

    def result_overwrite_file
      @files.each do |scriptfile|
	File.open( scriptfile.path, "w+" ) do |f|
	  f << scriptfile.new_script
	end
      end
    end

    def get_dumped_info
      work_dir_path = RRB.mk_work_dir
      begin
	@files.each do |scriptfile|
	  scriptfile.write_source_to( work_dir_path )
	end
	dirs = []
	Find.find( work_dir_path ) do |filepath|
	  dirs << filepath if FileTest.directory?( filepath )
	end
	run_file_path = File.join( work_dir_path, 'rrb_dump.rb' )
	run_file = File.open( run_file_path, "w" ) 
	dirs.each do |dirpath|
	  run_file << "$:.unshift '#{dirpath}'\n"
	end
	run_file << "require '#{@files[0].path[1..-1]}'\n"
	run_file << IO.read(File.join( File.dirname(__FILE__),"dump_modules.rb"))
	run_file.close
	IO.popen("ruby #{run_file_path}") do |io|
	  return DumpedInfo.get_dumped_info( io )
	end
      ensure
	FileUtils.rm_r work_dir_path
      end
    end
    
    def Script.new_from_io( input )

      files = []
      loop do	
	path = input.gets( IO_SPLITTER ).chop
	break if path == IO_TERMINATOR
	content = input.gets( IO_SPLITTER ).chop
	files << ScriptFile.new( StringIO.new( content ), path )
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

  module_function
  
  def mk_work_dir
    
    n = 0
    loop do
      raise_exeption = false
      path = WORK_DIR_BASENAME+'.'+Process.pid.to_s+'.'+n.to_s
      begin
	Dir.mkdir( path )
      rescue Errno::EEXIST
	raise_exeption = true
      end
      return path unless raise_exeption
      n += 1
    end
    
  end
  
end
