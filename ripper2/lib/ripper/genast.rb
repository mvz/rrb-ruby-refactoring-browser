=begin
Todo
The following forms need to be added

=end

require 'ripper'
require 'ripper/ast.rb'

class GenAst < Ripper
  attr_reader :code, :begin, :end

  def Ripper.parse(str, *args)
    new(str).parse(*args)
  end

  def initialize(str)
    super
    @code = []
    @begin = []
    @end = []
  end

  private

  #
  # Parser Events
  #

  #called for a begin block
  def on__BEGIN(code)
    @begin << code
    nil
  end

  #called for an end block
  def on__END(code)
    @end << code
    nil
  end

  #aliases the method with old_name to new_name
  def on__alias(new_name, old_name)
    Ruby::FunCall.new("alias_method", [new_name, old_name])   
  end

  #called at the end of the argument list
  #a = the result from the prior on__arglist_add calls
  def on__arg_paren(a)
    a
  end

  #called when adding an argument to an argument list
  #a = the current arglist
  #b = the new argument
  def on__arglist_add(a, b)
    return ( a << b)
  end

  def on__arglist_add_block(a, b)
    return a, b 
  end

  def on__arglist_add_star(a, b)
    return a,b 
  end

  #called when starting an argument list
  def on__arglist_new()
    []
  end

  def on__arglist_prepend(a, b)
    return a,b
  end

  #Called for an array literal, a contains the array
  def on__array(a)
    Ruby::ArrayLiteral.new(a)
  end
 
  def on__assoc_new(a, b)
    [a, b]
  end

  #a <op> b => a.<op>(b)
  #a, c = operands
  #b = operator
  def on__binary(a, b, c)
    Ruby::Call.new(a,b,[c])   
  end

  #called for var.<op>= value
  #which is translated into var = var <op> value
  def on__opassign(var, op, value)
    varref = on__var_ref(var)
    temp = Ruby::Call.new(varref,op,[value])   
    on__assign(var, temp)
  end

  #called for the code obj.var = val 
  def on__field(obj, sym, field)
    Ruby::Field.new(obj, field)      
  end

  #called for a for loop
  # for x in array code => array.each {|x| code}
  def on__for(var, array, code)
    call = Ruby::Call.new(array, "each", nil)
    Ruby::Iteration.new(call, [var], Ruby::Block.new(code))
  end

  #processes hash parameters then adds code to create an new has instance
  #by calling Hash[params]
  def on__hash(a)
    Ruby::HashLiteral.new(a)
  end

  #called for an if statement
  def on__if(conditional, body, elsecode)
    Ruby::If.new(conditional, body, elsecode)
  end

  #called for an else statement
  def on__else(code)
    code
  end

  #called for an elsif statement
  def on__elsif(conditional, body, elsecode)
    Ruby::If.new(conditional, body, elsecode)
  end

  #called for an unless statement
  def on__unless(conditional, body, elsecode)
    Ruby::Unless.new(conditional, body, elsecode)
  end

  #called for an if modifier
  def on__if_mod(conditional, body)
    Ruby::IfMod.new(conditional, body)
  end

  #called for an unless modifier
  def on__unless_mod(conditional, body)
    Ruby::UnlessMod.new(conditional, body)
  end

  #called when adding to a string
  #a = string being added to
  #b = string to add
  def on__string_add(a, b)
    a << b.to_s
    a
  end

  #called when starting a new string
  def on__string_content()
    ""
  end

  #called when a string has been finished
  #a = the string
  #"ab" => String.new(ab)
  def on__string_literal(a)
    Ruby::StringLiteral.new(a)    
  end

  #called for a new exectuable string `...`
  def on__xstring_new()
    ""
  end

  #called when adding text to an executable string
  def on__xstring_add(str, new)
    str << new
  end

  #called after processing an executable string `...`
  def on__xstring_literal(str)
    Ruby::XStringLiteral.new(str)
  end


  #called when a variable is assigned to
  def on__assign(var, value)
    if var.instance_of?(Ruby::Field)
      Ruby::Call.new(var.obj, var.id.to_s + "=", [value])
    else
      case var.to_s
      when /^@@/
        Ruby::ClassAssign.new(var, value)
      when /^@/
        Ruby::InstanceAssign.new(var, value)
      when /^\$/
        Ruby::GlobalAssign.new(var, value)
      when /^[A-Z]/
        Ruby::ConstDeclaration.new(var, value)
      else
        Ruby::LocalAssign.new(var, value)
      end
    end
  end

  #called when a variable is referenced
  def on__var_ref(var)
    case var.to_s
      when /^@@/
      Ruby::ClassVar.new(var)
      when /^@/
      Ruby::InstanceVar.new(var)
      when /^\$/
      Ruby::GlobalVar.new(var)
      when /^[A-Z]/
      Ruby::Const.new(var)
      else
      Ruby::LocalVar.new(var)
    end
  end
  
  def on__const_ref(const)
      #Ruby::Const.new(const)
      const
  end	

  #called when starting a new statement block
  def on__stmts_new()
      []
  end	

  #called at the end of a statement
  def on__stmts_add(a, b)
    unless a
      a = []
    end
    a <<  b
    @code = a
  end

  #method calls of the form obj.method(args)
  def on__call(obj, symbol, method)
    Ruby::Call.new(obj, method, nil)   
  end

  #method calls of the form obj.method args
  def on__command_call(obj, symbol, method, args)
    Ruby::Call.new(obj, method, args)     
  end

  #method calls of the form method(args)
  def on__fcall(method)
    Ruby::FunCall.new(method, nil)   
  end

  #method calls of the form method args
  def on__command(method, args)
    Ruby::FunCall.new(method, args)   
  end

  #called when arguments are added to a method call
  def on__method_add_arg(method, args)
    method.args = args
    method
  end

  def on__params(a, b, c, d)
   a
  end	

  def on__program(*args)
  end
  
  #called when defining a method
  def on__def(name, parameters, code)
    Ruby::Def.new(name, parameters, code)
  end

  #called when degining a singleton method
  def on__defs(klass, symbol, name, parameters, code)
    Ruby::Def.new(name, parameters, code, klass)
  end

  #called when defining a class
  def on__class(name, parent, code)
    Ruby::Klass.new(Ruby::Colon2.new(nil, name), parent, Ruby::Scope.new(code))
  end

  #called when defining a module
  def on__module(name, code)
    Ruby::Module.new(Ruby::Colon2.new(nil, name), Ruby::Scope.new(code))
  end

  #called for a block which is defined using braces
  def on__brace_block(parameters, body)
    [[parameters], Ruby::Block.new(body)]
  end

  #called for a block which is defined using do ... end
  def on__do_block(parameters, body)
    [[parameters], Ruby::Block.new(body)]
  end

  #called when a method is called with a block attached
  def on__iter_block(function, block)
    Ruby::Iteration.new(function, block[0], block[1])
  end


  #called for a while loop
  def on__while(condition, body)
    Ruby::While.new(condition, body)
  end

  #called for an until loop
  def on__until(condition, body)
    Ruby::Until.new(condition, body)
  end

  #called for a while modifier
  def on__while_mod(condition, body)
    Ruby::WhileMod.new(condition, body)
  end

  #called for an until modifier
  def on__until_mod(condition, body)
    Ruby::UntilMod.new(condition, body)
  end

  #called for a begin ... resuce block
  def on__begin(code)
    code
  end

  #called for a rescue block
  # rescue error => variable code
  def on__rescue(error, variable, code, next_block)
    Ruby::Rescue.new(error, variable, code, next_block)
  end

  #called for an ensure block
  def on__ensure(code)
    code
  end

  #a group of statements [def|class|mod] ... rescue ... else ... ensure ... end
  def on__bodystmt(code, rescueblocks, elseblock, ensureblock)
    Ruby::BodyStmt.new(code, rescueblocks, elseblock, ensureblock)
  end

  #called for a rescure modifier code rescue rescueblock
  def on__rescue_mod(rescueblock, code)
    Ruby::RescueMod.new(code, rescueblock)
  end

  #called for a case statement
  def on__case(expression, whenblocks)
    Ruby::Case.new(expression, whenblocks)
  end

  #called for a when statement
  #when value
  # code
  def on__when(value, code, nextblock)
    Ruby::When.new(value, code, nextblock)
  end

  #called or a regular expression /regexp/
  def on__regexp_literal(regexp)
    Ruby::RegexpLiteral.new(regexp)
  end

  def on__sp(token)
    token
  end
  
  def on__scan ( a,s )
    a
  end
  
  
  def on__ident ( a )
    a
  end
  
  
  def on__tstring_beg ( a )
    a
  end
  
  
  def on__tstring_content ( a )
    a
  end
  
  
  def on__tstring_end ( a )
    a
  end
  
  
  def on__nl ( a )
    a
  end
  
  
  def on__ignored_nl ( a )
    a
  end
  
  
  def on__kw ( a )
    a
  end
  
  
  def on__constpath_ref( parent,const )
    Ruby::Const.new( "#{parent.value}::#{const}".to_sym )
  end
  
  
  def on__const ( a )
    a
  end
  
  
  def on__op ( a )
    a
  end
  
  
  def on__lparen ( a )
    a
  end
  
  
  def on__rparen ( a )
    a
  end
  
  
  def on__paren ( a )
    a
  end

	
 	# Super with paretheses (and optionally arguments)
	def on__super ( params )
		Ruby::FunCall.new( :super, params )
	end
 
	# Super without parentheses (=> called with all arguments of the method)
  def on__zsuper ()
		Ruby::FunCall.new( :super, nil )
  end
  
  
  def on__var_field ( a )
    a
  end
  
  
  def on__period ( a )
    a
  end
  
  
  def on__comma ( a )
    a
  end
  
  
  def on__int ( int )
		Ruby::IntegerLiteral.new( int.to_i )
  end

	def on__float ( float )
		Ruby::FloatLiteral.new( float.to_f )
	end

	def on__return ( args )
		Ruby::Return.new( args )
	end

	def on__return0 ()
		Ruby::Return.new( nil )
	end

	def on__comment( comment )
		nil
	end

	def on__lbrace ( token )
		token
	end

	def on__rbrace ( token )
		token
	end

	def on__lbracket ( token )
		token
	end

	def on__rbracket ( token )
		token
	end

	# Range literal including "to" value
	def on__dot2 ( from,to )		
		Ruby::RangeLiteral.new( from, to, false )
	end

	# Range literal excluding "to" value
	def on__dot3 ( from, to )
		Ruby::RangeLiteral.new( from, to, true )
	end

	def on__block_var( token )
		token
	end

	def on__aref_field ( aref,args )
		Ruby::Call.new( aref, "[]", args )
	end

	def on__void_stmt ()
		nil
	end

	def on__hash ( a )
		Ruby::HashLiteral.new( a )
	end

	def on__symbeg ( a )
		a
	end

	def on__symbol( a )
		a
	end

	def on__symbol_literal( token )
		Ruby::SymbolLiteral.new( token )
	end

end

