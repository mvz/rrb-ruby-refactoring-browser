# ast.rb - classes for representing Ruby abstract syntax trees
#
# Copyright 2003 (c) Robert Feldt, feldt@ce.chalmers.se
#

module Parse
  class Tree
    include Enumerable
    
    def Tree.new_tree_class(name, children_names = [])
      name_as_str = name.to_s
      unless name_as_str =~ /^[A-Z]/
	raise NameError, "identifier #{name} needs to be constant"
      end
      unless constants.include?(name_as_str)
	s = <<-EOC
        class #{name_as_str} < #{self.inspect}
	    #{child_accessor_codes(children_names)}
	end
	EOC
	class_eval s
      end
      class_eval "#{name_as_str}"
    end

    def Tree.child_accessor_codes(children_names)
      i, s = 0, ""
      while i < children_names.length
	s += child_accessor_code(children_names[i], i)
	i += 1
      end
      s
    end

    def Tree.child_accessor_code(children_name, index)
      name = children_name.to_s
      s = <<-EOC
        def #{name}
          @children[#{index}]
        end
	def #{name}=(newValue)
	  @children[#{index}] = newValue
        end
      EOC
    end

    def Tree.[](*children)
      new(*children)
    end

    attr_reader :children

    def initialize(*children)
      @@level = 0
      @children = children
    end

    def [](index)
      @children[index]
    end

    def num_children
      @children.length
    end

    def ==(other)
      self.class == other.class &&
	self.children == other.children
    end

    def inspect
      class_name + "[" +
        self.children.map {|c| c.inspect}.join(", ") + 
        "]"
    end

    def to_s
      string = class_name
      if self.children.length == 1
        string += "(#{self.children[0].inspect})"
      else
        string +=  self.children.map do |c| 
          @@level+=1
          temp = ("\n" + "  " * @@level) + c.to_s  
          @@level-=1
          temp
        end.join(" ")
      end
    end

    def class_name
      self.class.inspect.split("::").last
    end

    def each
      @children.each {|c| yield c}
    end

    def match_tree(tree, matches = Hash.new)
      if self.class == tree.class && 
	  self.num_children == tree.num_children
        self.each_with_index do |child, i|
	  unless child.match_tree(tree[i], matches)
	    matches.clear
	    break
	  end
        end
        matches
      else
        nil
      end
    end

    def =~(patternTree)
      unless Tree::Base === patternTree
	raise ArgumentError, "pattern must be a Tree::Base"
      end
      pattern = patternTree.upcase_symbols_to_pattern_vars
      pattern.match_tree(self, m = Hash.new)
    end
  
    class PatternVar
      attr_reader :name

      def initialize(name)
	@name = name
      end

      def match_tree(pattern, matches = Hash.new)
	matches[@name] = pattern
	matches
      end

      def inspect
	"V_" + @name.to_s
      end
    end

    def upcase_symbols_to_pattern_vars    
      new_children = Array.new
      self.each {|child| new_children << child.upcase_symbols_to_pattern_vars}
      self.class.new(*new_children)
    end
  end

  class Object
    def match_tree(pattern, matches = Hash.new)
      self == pattern
    end
    
    def upcase_symbols_to_pattern_vars
      self
    end
  end

  class Symbol
    def upcase_symbols_to_pattern_vars
      if self.to_s =~ /^[A-Z]/
	Tree::PatternVar.new(self)
      else
	self
      end
    end
  end
end

# NOTE! This is very incomplete. Just wanted to give an idea of how
# it could work...

module Ruby
  # currently uses Parse::Tree from Rockit but we include its source with Ruth
  # for now so that people don't need to install rockit just to get ruth to
  # work...
  class AST < Parse::Tree
    # Attribute for holding the start position in the source code for
    # the tree node.
    attr_accessor :pos
  end

  # helper function for creating new Ruby::AST subclasses
  def Ruby.ast(name, *children_names)
    AST.new_tree_class(name, children_names)
  end

  # helper function for creating new subclasses from a Ruby::AST subclass
  def Ruby.ast_sub(superclass, name, *children_names)
    superclass.new_tree_class(name, children_names)
  end

  #-------------------------------------------------------------------------
  # Ruby::AST classes for representing the language
  #-------------------------------------------------------------------------

  # Base
  Self          = ast(:Self)
  Alias         = ast(:Alias,      :to, :from)
  BeginBlock    = ast(:BeginBlock, :body)
  EndBlock      = ast(:EndBlock,   :body)
  Return        = ast(:Return,     :mvalue)
  Yield         = ast(:Yield,      :mvalue)

  # Literals
  Literal        = ast(:Literal)   # superclass for all literals

  # Atomic literals have no value
  AtomicLiteral  = ast_sub(Literal, :AtomicLiteral)
  NilLiteral     = ast_sub(AtomicLiteral, :NilLiteral)
  FalseLiteral   = ast_sub(AtomicLiteral, :FalseLiteral)
  TrueLiteral    = ast_sub(AtomicLiteral, :TrueLiteral)

  # Value literals have a single value
  ValueLiteral   = ast_sub(Literal, :ValueLiteral, :value)
  IntegerLiteral = ast_sub(ValueLiteral, :IntegerLiteral)
  FloatLiteral   = ast_sub(ValueLiteral, :FloatLiteral)
  SymbolLiteral  = ast_sub(ValueLiteral, :SymbolLiteral)
  StringLiteral  = ast_sub(ValueLiteral, :StringLiteral)
  XStringLiteral = ast_sub(ValueLiteral, :XStringLiteral)
  ArrayLiteral   = ast_sub(ValueLiteral, :ArrayLiteral)
  HashLiteral    = ast_sub(ValueLiteral, :HashLiteral)
  RegexpLiteral  = ast_sub(ValueLiteral, :RegexpLiteral)

  RangeLiteral   = ast( :RangeLiteral, :from, :to, :exclude_end )

  # Loop-related
  For           = ast(:For,        :variables, :body)
  Break         = ast(:Break)
  Redo          = ast(:Redo)
  Next          = ast(:Next)

  # Assignments
  Assignment    = ast(:Assignment)
  LocalAssign   = ast_sub(Assignment, :LocalAssign, :id, :value)  
  InstanceAssign = ast_sub(Assignment, :InstanceAssign, :id, :value)
  ClassAsign	= ast_sub(Assignment, :ClassAssign, :id, :value)
  Field         = ast(:Field, :obj, :id)
  
  # Method call
  Call          = ast(:Call,     :obj, :method, :args)

  # Fun call
  FunCall       = ast(:FunCall,  :method, :args)

  # Method def
  Def           = ast(:Def,      :name, :args, :body)

  # Local variable reference
  LocalVar      = ast(:LocalVar, :name)

  #instance variable reference
  InstanceVar      = ast(:InstanceVar, :name)

  #class variable reference
  ClassVar 	= ast(:ClassVar, :name)
  # A block of statements
  Block         = ast(:Block)

  # Class, we call it Klass simply to distinghuish from the usual Class class
  Klass         = ast(:Klass,    :name, :superclass, :body)

  Module         = ast(:Module,    :name, :body)

  # Scope for a body of statements
  Scope         = ast(:Scope,    :vars, :statements)

  # Colon2 is for sequences of constants like A::B::C. Find better name?
  Colon2        = ast(:Colon2, :parent, :constant)

  Const         = ast(:Const, :value)

  GlobalVar     = ast(:GlobalVar, :name)
  GlobalAssign  = ast(:GlobalAssign, :name, :value) # This should probably be just Assign?

  # Operators
  BinOp         = ast(:BinOp, :left, :right)
  Or            = ast_sub(BinOp, :Or)
  And           = ast_sub(BinOp, :And)

  # Nodes below just added as counterparts to internal ones. They need
  # to be simplified to give meaningful high-level AST!!!

  DynAssignCurrent = ast(:DynAssignCurrent, :name, :value)

  Iteration = ast(:Iteration, :object, :args, :body)

  DynVar = ast(:DynVar, :name)

  If = ast(:If, :conditional, :body, :else)
  IfMod = ast(:IfMod, :conditional, :body)

  Unless = ast(:Unless, :conditional, :body, :else)
  UnlessMod = ast(:UnlessMod, :conditional, :body)

  While = ast(:While, :conditional, :body)
  WhileMod = ast(:WhileMod, :conditional, :body)

  Until = ast(:Until, :conditional, :body)
  UntilMod = ast(:UntilMod, :conditional)

  ConstDeclaration = ast(:ConstDeclaration, :name, :value)

  RegexpMatch = ast(:RegexpMatch, :regexp, :string)

  # For $1 etc. Not sure what it should really be called.
  NthRef = ast(:NthRef, :num)

  # How is VCall different from Call?
  VCall = ast(:VCall, :method)

  DStr = ast(:Dstr, :literal, :next)
  EvalStr = ast(:EvalStr, :string)

  BodyStmt = ast(:BodyStmt, :code, :rescueblock, :elseblock, :ensureblock)
  Rescue   = ast(:Rescue, :error, :variable, :code, :nextblock)
  RescueMod = ast(:RescueMod, :code, :rescueblock)

  Case = ast(:Case, :expression, :whenblocks)
  When = ast(:When, :value, :code, :nextblock)
end
