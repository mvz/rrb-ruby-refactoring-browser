require 'rrb/ripper'
require 'rrb/utils'
require 'rrb/node'

module RRB
      
  class ParseError < RRBError
  end
  
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
	@assigned = []
	@attr_readers = []
	@attr_writers = []
	@attr_accessors = []
      end
      attr_reader :class_defs, :method_defs, :method_calls, :local_vars, :fcalls
      attr_reader :global_vars, :instance_vars, :class_vars, :consts
      attr_reader :singleton_method_defs, :class_method_defs
      attr_reader :singleton_class_defs
      attr_reader :assigned
      attr_reader :attr_readers, :attr_writers, :attr_accessors
    end

    def initialize(file, path="")
      @path = path
      super 
    end
    
    # parse and return tree
    def run
      @scope_stack = Array.new
      @scope_stack.push Scope.new
      
      self.parse
      TopLevelNode.new(@scope_stack[0])
    end

    def on__parse_error(msg)
      raise(ParseError, "#{@path}:#{lineno}: #{msg}")
    end
    
    def on__const(token)
      IdInfo.new(:const, lineno, column + token.size, token)
    end

    def on__ident(token)
      IdInfo.new(:id, lineno, column + token.size, token)
    end

    def on__cvar(token)
      IdInfo.new(:cvar, lineno, column + token.size, token)
    end

    def on__ivar(token)
      IdInfo.new(:ivar, lineno, column + token.size, token)
    end

    def on__gvar(token)
      IdInfo.new(:gvar, lineno, column + token.size, token)
    end

    def on__kw(token)
      IdInfo.new(:keyword, lineno, column + token.size, token)
    end
    
    def on__op(token)
      IdInfo.new(:op, lineno, column + token.size, token)
    end
    
    def on__symbol(id)
      IdInfo.new(:symbol, id.lineno, id.pointer, id.name)
    end


    def on__local_push
      @scope_stack.push(Scope.new)
    end

    def on__local_pop
      @scope_stack.pop
    end

    def add_var( var )
      case var.type
      when :id
	@scope_stack.last.local_vars << var
        return var
      when :gvar
	@scope_stack.last.global_vars << var
        return var
      when :ivar
	@scope_stack.last.instance_vars << var
        return var
      when :cvar
	@scope_stack.last.class_vars << var
        return var
      when :const
	const = ConstInfo.new_normal( var )
	@scope_stack.last.consts << const
	return const
      when :keyword
        return var
      end      
    end
    
    def on__var_ref(var)
      return unless var.kind_of?( IdInfo )
      if @scope_stack.last.local_vars.find{|i| i.name == var.name } then
	@scope_stack.last.local_vars << var
        return var
      elsif var.type == :id
        method_call = MethodCall.new(var, [])
	@scope_stack.last.fcalls << method_call
        return method_call
      else
	return add_var( var )
      end
    end

    def on__var_field(var)
      return unless var.kind_of?(IdInfo)
      @scope_stack.last.assigned << var if var.type == :id
      add_var(var)
    end
    
    def on__params(params, opt_params, rest, block)
      @scope_stack.last.local_vars.concat(params) if params
      @scope_stack.last.local_vars.concat(opt_params.map{|assoc| assoc[0]}) if opt_params
      @scope_stack.last.local_vars << rest if rest
      @scope_stack.last.local_vars << block if block

      params || []
    end

    def on__restparam(rest)
      rest
    end
    
    def on__constpath(lconst, const_id)
      return nil unless lconst.kind_of?(ConstInfo)
      const = ConstInfo.new_colon2(const_id, lconst)
      @scope_stack.last.consts << const
      return const
    end
    
    def on__constpath_ref(lconst, const_id)
      on__constpath(lconst, const_id)
    end

    def on__constpath_field(lconst, const_id)
      on__constpath(lconst, const_id)
    end

    def on__topconst(const_id)
      const = ConstInfo.new_toplevel(const_id)
      @scope_stack.last.consts << const
      return const
    end

    def on__topconst_field(const_id)
      on__topconst(const_id)
    end

    def on__topconst_ref(const_id)
      on__topconst(const_id)
    end

    def on__const_ref(const_id)
      const_id
    end

    def add_attr(function, args)
      return unless args.kind_of?(Array)
      case function.name
      when 'attr_reader'
	@scope_stack.last.attr_readers.concat args
      when 'attr_writer'
	@scope_stack.last.attr_writers.concat args	  
      when 'attr_accessor'
	@scope_stack.last.attr_accessors.concat args
      end
    end
    
    def on__call(reciever, call_type, method, args)
      @scope_stack.last.method_calls << MethodCall.new(method, args || []) 
    end

    def on__fcall(method, args)
      @scope_stack.last.fcalls << MethodCall.new(method, args || [])
      add_attr(method, args)
    end

    def on__command_call(reciever, call_type, operation, args)
      @scope_stack.last.method_calls << MethodCall.new(operation, args || [])
    end

    def on__command(operation, args)
      @scope_stack.last.fcalls << MethodCall.new(operation, args || [])
      add_attr(operation, args)
    end
    
    def on__class(kw_class, cpath, superclass, stmts, kw_end)
      unless cpath.kind_of?(IdInfo)
        raise RRBError, "This version doesn't support classname with scope operator"
      end
      @scope_stack.last.singleton_method_defs.delete_if do |sdef|
	if sdef.s_obj.name == cpath.name || sdef.s_obj.self?
	  @scope_stack.last.class_method_defs << ClassMethodNode.new(sdef)
	  @scope_stack.last.consts.delete(sdef.s_obj)
	  true
	else
	  false
	end
      end
      
      @scope_stack[-2].class_defs << ClassNode.new(cpath,
                                                   @scope_stack.last,
                                                   superclass,
						   kw_class, kw_end)
    end

    def on__module(kw_module, mpath, stmts, kw_end)
      unless mpath.kind_of?(IdInfo)
        raise RRBError, "This version doesn't support modulename with scope operator"
      end
      @scope_stack[-2].class_defs << ModuleNode.new(mpath,
						    @scope_stack.last,
						    kw_module, kw_end )
    end

    def on__sclass(kw_class, s_obj, stmts, kw_end)
      @scope_stack[-2].singleton_class_defs <<
	SingletonClassNode.new(IdInfo.new(:nil, 0, 0, "[sclass]"),
                               @scope_stack.last, kw_class, kw_end)
    end
    
    def on__def(kw_def, name, params, stmts, kw_end)
      @scope_stack[-2].method_defs <<
	MethodNode.new(name, @scope_stack.last, params, kw_def, kw_end)
    end

    def on__defs(kw_def, s_obj, type, name, params, stmts, kw_end)
      s_obj = IdInfo.new( :nil, 0, 0, "" ) if s_obj == nil
      @scope_stack[-2].singleton_method_defs <<
	SingletonMethodNode.new(s_obj, name, @scope_stack.last,
				params, kw_def, kw_end)
    end
    
    def on__arglist_new
      Array.new
    end

    def on__arglist_add(args, arg)
      return nil unless args.kind_of?( Array )
      return nil unless arg.kind_of?( IdInfo )
      args << arg
    end

    def on__arglist_add_block(args, block)
      args
    end

    def on__arglist_add_star(args, arg)
      args
    end

    def on__arg_paren(args)
      args
    end

    def on__arglins_prepend(arg, args)
      on__arglist_add(args, arg)
    end

    def on__space(args)
      args
    end

    def on__paren(params)
      params
    end
    
    def on__symbol_literal(literal)
      literal
    end
  end
end

  
