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
						   @scope_stack.top )
    end

    def on__module( module_name, stmts )
      @scope_stack[-2].class_defs << ModuleNode.new( module_name,
						    @scope_stack.top )
    end
    
    def on__def( def_str, name, arglist, stmts, rescue_clause )
      @scope_stack[-2].method_defs <<
	MethodNode.new( name, @scope_stack.top )	
    end

    def on__call( receiver, method, args )
      @scope_stack.top.method_calls << method
    end

    def on__fcall( function, args )
      @scope_stack.top.fcalls << function
    end

    def on__varcall( method, arg )
      @scope_stack.top.fcalls << method
    end

    def on__assignable( var, arg )
      @scope_stack.top.local_vars << var
    end

    def on__local_count(  arg )
      return if arg == '~'.intern
      @scope_stack.top.local_vars << arg
    end
    
    def on__varref( var )
      if @scope_stack.top.local_vars.find{|i| i.name == var.name } then
	@scope_stack.top.local_vars << var
      elsif var.type == :id
	@scope_stack.top.fcalls << var
      end
    end
    
  end
  
  
end
