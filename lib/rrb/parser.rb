require 'rrb/ripper'
require 'rrb/utils'
require 'rrb/node'

module RRB
      
  class Parser < Ripper

    class Scope
      def initialize
	@class_defs = []
	@method_defs = []
	@method_calls = []
	@local_vars = []
	@fcalls = []
      end
      attr_reader :class_defs, :method_defs, :method_calls, :local_vars, :fcalls
    end
    
    # parse and return tree
    def run( file )
      @scope_stack = Array.new
      @scope_stack.push Scope.new
      self.parse( file )

      TopLevelNode.new( @scope_stack[0] )
    end

    # on constant
    def on__CONSTANT( id )
      IdInfo.new :const, lineno, pointer, id
    end

    # on normal identifier
    def on__IDENTIFIER( id )
      IdInfo.new :id, lineno, pointer, id
    end

    # on class variable 
    def on__CVAR( id )
      IdInfo.new :cvar, lineno, pointer, id
    end

    # on function identifier like "has_key?" or "map!"
    def on__FID( id )
      IdInfo.new :fid, lineno, pointer, id
    end

    # on instance varaible
    def on__IVAR( id )
      IdInfo.new :ivar, lineno, pointer, id
    end

    # on global variable
    def on__GVAR( id )
      IdInfo.new :gvar, lineno, pointer, id
    end

    def on__local_push
      @scope_stack.push( Scope.new )
    end

    def on__local_pop
      @scope_stack.pop
    end

    def on__class( class_name, stmts, superclass )
      @scope_stack[-2].class_defs << ClassNode.new( class_name,
						   @scope_stack.last )
    end

    def on__module( module_name, stmts )
      @scope_stack[-2].class_defs << ModuleNode.new( module_name,
						    @scope_stack.last )
    end
    
    def on__def( def_str, name, arglist, stmts, rescue_clause )
      @scope_stack[-2].method_defs <<
	MethodNode.new( name, @scope_stack.last )	
    end

    def on__call( receiver, method, args )
      @scope_stack.last.method_calls << method
    end

    def on__fcall( function, args )
      @scope_stack.last.fcalls << function
    end

    def on__varcall( method, arg )
      @scope_stack.last.fcalls << method
    end

    def on__assignable( var, arg )
      @scope_stack.last.local_vars << var
    end

    def on__local_count(  arg )
      return if arg == '~'.intern
      @scope_stack.last.local_vars << arg
    end
    
    def on__varref( var )
      if @scope_stack.last.local_vars.find{|i| i.name == var.name } then
	@scope_stack.last.local_vars << var
      elsif var.type == :id
	@scope_stack.last.fcalls << var
      end
    end

    def on__add_eval_string( context, str )
      @eval_str = str
    end

    def on__eval_string_end( context, str )
      if str == "}"
	mcalls, fcalls, lvars =
	  EvalStringParser.new.run( @eval_str, @scope_stack.last, lineno,
				   pointer - @eval_str.size - 1  )
      else
	mcalls, fcalls, lvars =
	  EvalStringParser.new.run( @eval_str, @scope_stack.last, lineno,
				   pointer - @eval_str.size )
      end
      @scope_stack.last.method_calls.concat mcalls
      @scope_stack.last.fcalls.concat fcalls
      @scope_stack.last.local_vars.concat lvars
    end
    
  end
  
  class EvalStringParser < Parser

    def adjust_id( ids, lineno, pointer )
      ids.map do |id|
	if id.lineno > 1 then
	  p id
	  raise RRBError, "eval string mustn't have \"\\n\""
	end
	IdInfo.new( id.type, lineno, id.pointer + pointer, id.name )
      end
    end
    
    def run( input, scope, lineno, pointer )
      @scope_stack = Array.new
      @scope_stack.push Marshal.load( Marshal.dump(scope) )
      self.parse( input )

      method_calls = @scope_stack[0].method_calls[scope.method_calls.size..-1]
      fcalls = @scope_stack[0].fcalls[scope.fcalls.size..-1]
      local_vars = @scope_stack[0].local_vars[scope.local_vars.size..-1]

      method_calls = adjust_id( method_calls, lineno, pointer )
      fcalls = adjust_id( fcalls, lineno, pointer )
      local_vars = adjust_id( local_vars, lineno, pointer ) 
      return method_calls, fcalls, local_vars
    end
    
  end
  
end
