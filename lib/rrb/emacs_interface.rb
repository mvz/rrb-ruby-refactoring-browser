require 'rrb/script'
require 'rrb/rename_local_var'
require 'rrb/rename_instance_var'
require 'rrb/rename_class_var'
require 'rrb/rename_global_var'
require 'rrb/rename_method_all'
require 'rrb/extract_method'
require 'rrb/move_method'

module RRB

  class EmacsInterface

    USAGE = "\
Usage: rrb refactoring-type refactoring-parameter io-type

  refactoring-type
    * --rename-local-variable  Class#method old_var new_var
    * --rename-method-all  old_method new_method
    * --rename-class-variable  Class old_var new_var
    * --rename-instance-variable  Class old_var new_var
    * --rename-global-variable  old_var new_var
    * --extract-method path new_method start_lineno end_lineno
    * --rename-method old_method new_method classes...

  io-type
    * --stdin-stdout
    * --filein-overwrite FILES..
    * --filein-stdout FILES..
"
    
    def initialize( argv )
      parse_argv argv
    end

    def split_method_name( str )
      a, b = str.split( /#/ )
      return Namespace.new(a), b
    end

    def parse_argv( argv )

      # analyze REFACTORING-TYPE
      case argv.shift
      when '--rename-local-variable'
	namespace, method_name = split_method_name argv.shift
	old_var = argv.shift
	new_var = argv.shift
	@args = [ namespace, method_name, old_var, new_var ]
	@refactoring_method = :rename_local_var
	@check_method = :rename_local_var?
      when '--rename-instance-variable'
	namespace = Namespace.new( argv.shift )
	old_var = argv.shift
	new_var = argv.shift
	@args = [ namespace, old_var, new_var ]
	@refactoring_method = :rename_instance_var
	@check_method = :rename_instance_var?
      when '--rename-class-variablnae'
	namespace = Namespace.new( argv.shift )
	old_var = argv.shift
	new_var = argv.shift
	@args = [ namespace, old_var, new_var ]
	@refactoring_method = :rename_class_var
	@check_method = :rename_class_var?
      when '--rename-global-variable'
	old_var = argv.shift
	new_var = argv.shift
	@args = [ old_var, new_var ]
	@refactoring_method = :rename_global_var
	@check_method = :rename_global_var?
      when '--rename-method-all'
	old_method = argv.shift
	new_method = argv.shift
	@args = [ old_method, new_method ]
	@refactoring_method = :rename_method_all
	@check_method = :rename_method_all?
      when '--extract-method'
	filepath = argv.shift
	new_method = argv.shift
	start_lineno = argv.shift.to_i
	end_lineno = argv.shift.to_i
	@args = [ filepath, new_method, start_lineno, end_lineno ]
	@refactoring_method = :extract_method
	@check_method = :extract_method?
      when '--move-method'
	method_name = argv.shift
	old_namespace = Namespace.new( argv.shift )
	new_namespace = Namespace.new( argv.shift )
	@args = [ method_name, old_namespace, new_namespace ]
	@refactoring_method = :move_method
	@check_method = :move_method?	
      when '--rename-method'
	old_method = argv.shift
	new_method = argv.shift
	for i in 0...(argv.size)
	  break if /^--/ =~ argv[i]
	end
	classes = argv[0,i]
	i.times{ argv.shift }
	@args = [ classes, old_method, new_method ]
	@refactoring_method = :rename_method
	@check_method = :rename_method?
      else
	raise RRBError, "Unknown refactoring"
      end

      # analyze IO-TYPE
      case argv.shift
      when '--stdin-stdout', nil
	@script = Script.new_from_io( STDIN )
	@output = proc{ @script.result_to_io( STDOUT ) }
      when '--filein-overwrite'      
	@script = Script.new_from_filenames( argv )
	@output = proc{ @script.result_rewrite_file }
      when '--filein-stdout'
	@script = Script.new_from_filenames( argv )
	@output = proc{ @script.result_to_io( STDOUT ) }
      else
	raise RRBError, "Unknown input/output option"
      end
      
    end

    def enable_refactor?
      @script.__send__ @check_method, *@args
    end

    def refactor
      @script.__send__ @refactoring_method, *@args
    end

    def output
      @output.call
    end
    
  end

  
end
