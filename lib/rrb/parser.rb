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
	@global_vars = []
	@instance_vars = []
	@class_vars = []
	@consts = []
	@fcalls = []
	@singleton_method_defs = []
	@class_method_defs = []
	@singleton_class_defs = []      
      end
      attr_reader :class_defs, :method_defs, :method_calls, :local_vars, :fcalls
      attr_reader :global_vars, :instance_vars, :class_vars, :consts
      attr_reader :singleton_method_defs, :class_method_defs
      attr_reader :singleton_class_defs
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

    def on__OP( op )
      IdInfo.new :op, lineno, pointer, op
    end
    
    def on__local_push
      @scope_stack.push( Scope.new )
    end

    def on__local_pop
      @scope_stack.pop
    end

    def on__class( class_name, stmts, superclass )

      @scope_stack.last.singleton_method_defs.delete_if do |sdef|
	if sdef.s_obj.name == class_name.name then
	  @scope_stack.last.class_method_defs << ClassMethodNode.new( sdef )
	  @scope_stack.last.consts.delete( sdef.s_obj )
	  true
	else
	  false
	end
      end
      
      @scope_stack[-2].class_defs << ClassNode.new( class_name,
						   @scope_stack.last )
    end

    def on__module( module_name, stmts )
      @scope_stack[-2].class_defs << ModuleNode.new( module_name,
						    @scope_stack.last )
    end

    def on__sclass( s_obj, stmts )
      @scope_stack[-2].singleton_class_defs <<
	SingletonClassNode.new( IdInfo.new( :nil, 0, 0, "[sclass]" ),
			       @scope_stack.last )
    end
    
    def on__def( def_str, name, arglist, stmts, rescue_clause )
      @scope_stack[-2].method_defs <<
	MethodNode.new( name, @scope_stack.last )	
    end

    def on__sdef( s_obj, method_name, arglist, stmts )
      s_obj = IdInfo.new( :nil, 0, 0, "" ) if s_obj == nil
      @scope_stack[-2].singleton_method_defs <<
	SingletonMethodNode.new( s_obj, method_name, @scope_stack.last )
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

    def add_var( var )
      case var.type
      when :id
	@scope_stack.last.local_vars << var
      when :gvar
	@scope_stack.last.global_vars << var
      when :ivar
	@scope_stack.last.instance_vars << var
      when :cvar
	@scope_stack.last.class_vars << var
      when :const
	const = ConstInfo.new_normal( var )
	@scope_stack.last.consts << const
	return const
      end      
    end

    def on__assignable( var, arg )
      return unless var.kind_of?( IdInfo )
      add_var( var )
    end

    def on__local_count(  arg )
      return if arg == '~'.intern
      @scope_stack.last.local_vars << arg
    end
    
    def on__varref( var )
      return unless var.kind_of?( IdInfo )

      if @scope_stack.last.local_vars.find{|i| i.name == var.name } then
	@scope_stack.last.local_vars << var
      elsif var.type == :id
	@scope_stack.last.fcalls << var
      else
	return add_var( var )
      end
    end

    def on__toplevel_const_get( const_id )
      const = ConstInfo.new_toplevel( const_id )
      @scope_stack.last.consts << const
      return const
    end

    def on__const_get( lconst, const_id )
      return nil unless lconst.kind_of?( ConstInfo )
      const = ConstInfo.new_colon2( const_id, lconst )
      @scope_stack.last.consts.delete( lconst )
      @scope_stack.last.consts << const
      return const
    end
    
    def on__add_eval_string( context, str )
      @eval_str = str
    end

    def on__eval_string_end( context, str )
      if str == "}"
	mcalls, fcalls, lvars, gvars, ivars, cvars =
	  EvalStringParser.new.run( @eval_str, @scope_stack.last, lineno,
				   pointer - @eval_str.size - 1  )
      else
	mcalls, fcalls, lvars, gvars, ivars, cvars =
	  EvalStringParser.new.run( @eval_str, @scope_stack.last, lineno,
				   pointer - @eval_str.size )
      end
      @scope_stack.last.method_calls.concat mcalls
      @scope_stack.last.fcalls.concat fcalls
      @scope_stack.last.local_vars.concat lvars
      @scope_stack.last.global_vars.concat gvars
      @scope_stack.last.instance_vars.concat ivars
      @scope_stack.last.class_vars.concat cvars
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
      # push current scope to distinguish local variable from function call
      new_scope = Scope.new
      new_scope.local_vars.concat( scope.local_vars )
      @scope_stack.push new_scope
      
      self.parse( input )

      method_calls = @scope_stack[0].method_calls
      fcalls = @scope_stack[0].fcalls
      local_vars = @scope_stack[0].local_vars[scope.local_vars.size..-1]
      global_vars = @scope_stack[0].global_vars
      instance_vars = @scope_stack[0].instance_vars
      class_vars = @scope_stack[0].class_vars
      
      method_calls = adjust_id( method_calls, lineno, pointer )
      fcalls = adjust_id( fcalls, lineno, pointer )
      local_vars = adjust_id( local_vars, lineno, pointer )
      global_vars = adjust_id( global_vars, lineno, pointer )
      instance_vars = adjust_id( instance_vars, lineno, pointer )
      class_vars = adjust_id( class_vars, lineno, pointer )
      
      return method_calls, fcalls, local_vars, global_vars, instance_vars,
	class_vars
    end
    
  end
  
end
