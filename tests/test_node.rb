require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/parser'

class TestNode < RUNIT::TestCase

  TEST_SCRIPT_NAME = 'samples/visitor_sample.rb'

  class Visitor1 < RRB::Visitor
    
    def initialize
      @classes = []
      @methods = []
    end
      
    def visit_method( namespace, method_node )
      @methods << [ namespace.last.name, method_node.name ]
    end
    
    def visit_class( namespace, class_node )
      @classes <<  class_node.name 
    end
      
    attr_reader :classes, :methods      
  end

  class Visitor2 < RRB::Visitor

    def initialize
      @classes = []
    end

    attr_reader :classes
    
    def visit_class( namespace, class_node )
      @classes << [ namespace.map{|i| i.name}, class_node.name ]
    end
    
  end

  class Visitor3 < RRB::Visitor

    def visit_toplevel( namespace, top_node )
      @top_classes = top_node.class_defs.map{|i| i.name}
    end
    attr_reader :top_classes
    
  end

  class Visitor4 < RRB::Visitor

    def initialize
      @nodes = []
    end

    attr_reader :nodes
    def visit_node( namespace, node )
      @nodes << namespace.map{|i| i.name}.push( node.name )
    end

    
  end

  class Visitor5 < RRB::Visitor
    def initialize
      @singleton_methods = []
    end

    def visit_singleton_method( namespace, node )
      @singleton_methods << [ namespace.map{|i| i.name}, node.name ]
    end
    attr_reader :singleton_methods
  end

  class Visitor6 < RRB::Visitor
    def initialize
      @class_methods = []
    end

    def visit_class_method( namespace, node )
      @class_methods << [ namespace.map{|i| i.name}, node.name ]
    end
    attr_reader :class_methods
  end
  
  def test_accept

    parser = RRB::Parser.new
    tree = parser.run File.open( TEST_SCRIPT_NAME, "r" )

    # test visit_class and visit_method
    visitor1 = Visitor1.new
    tree.accept( visitor1 )
    assert_equals( [ 'TestClassA', 'TestClassB', 'TestClassC',
		    'TestModuleA', 'TestModuleB' ],
		  visitor1.classes )
    assert_equals( [[ 'TestClassA', 'method_1'],
		    [ 'TestClassA', 'method_2'],
		    [ 'TestClassA', 'method_3'],
		    [ 'TestClassB', 'method_4'],
		    [ 'TestModuleB', 'method_5'],
		    [ '[sclass]', 'method_8' ],
		  ],
		  visitor1.methods.sort )

    # test visit_class and namespace
    visitor2 = Visitor2.new
    tree.accept( visitor2 )
    assert_equals( [ [[], 'TestClassA'],
		    [['TestClassA'],'TestClassB'],
		    [['TestClassA','TestClassB'],'TestClassC'],
		    [['TestClassA'],'TestModuleA'],
		    [['TestClassA','TestModuleA'],'TestModuleB'],
		  ],
		  visitor2.classes )

    # test visit_toplevel
    visitor3 = Visitor3.new
    tree.accept( visitor3 )
    assert_equals( [ 'TestClassA' ], visitor3.top_classes )

    # test visit_node
    visitor4 = Visitor4.new
    tree.accept( visitor4 )    
    assert_equals( [['toplevel'],
		    ['TestClassA'],
		    ['TestClassA','method_1'],
		    ['TestClassA','method_2'],
		    ['TestClassA','method_3'],
		    ['TestClassA','TestClassB'],
		    ['TestClassA','TestClassB','method_4'],
		    ['TestClassA','TestClassB','TestClassC'],
		    ['TestClassA','TestModuleA'],
		    ['TestClassA','TestModuleA','TestModuleB'],
		    ['TestClassA','TestModuleA','TestModuleB','method_5'],
		    ['TestClassA','method_6'],
		    ['TestClassA','method_7'],
		    ['TestClassA', '[sclass]' ],
		    ['TestClassA', '[sclass]', 'method_8' ],
		  ].sort, visitor4.nodes.sort )

    # test visit_singleton_method
    visitor5 = Visitor5.new
    tree.accept( visitor5 )
    assert_equals( [ [['TestClassA'],'method_6'] ].sort,
		  visitor5.singleton_methods.sort )

    # test visit_class_method
    visitor6 = Visitor6.new
    tree.accept( visitor6 )
    assert_equals( [ [['TestClassA'],'method_7'] ].sort,
		  visitor6.class_methods.sort )

  end

  def test_range
    parser = RRB::Parser.new
    tree = parser.run File.open( TEST_SCRIPT_NAME, "r" )

    def_range = tree.class_info( "TestClassA" ).range
    assert_equals( 2, def_range.head.lineno )
    assert_equals( 51, def_range.tail.lineno )
    assert( def_range.contain?( 3..50 ) )
    assert( !def_range.contain?( 2..50 ) )
    assert( !def_range.contain?( 3..51 ) )
    assert( def_range.out_of?( 52..55 ) )
    assert( !def_range.out_of?( 3..50 ) )
    assert( !def_range.out_of?( 1..2 ) )
    assert( !def_range.out_of?( 51..55 ) )
  end
  
end

class TestConstInfo < RUNIT::TestCase

  TEST_SCRIPT_NAME = 'samples/parser_sample.rb'
  def test_body
    parser = RRB::Parser.new
    tree = parser.run File.open( TEST_SCRIPT_NAME, "r" )
    consts = tree.class_info("TestClassA").method_info("method_9").consts
    assert_equals( [ "Const1",
		    "Const2", "Const1",
		    "Const2", "Const1",
		    "Const4", "Const5",
		    "Const6", "Const7",
		    "Const8", "Const9",
		    "Const10", "Const11",
		    "Const12"
		  ],
                   consts.map{|const| const.body.name} )
  end
end
class TestMethod < RUNIT::TestCase
  class Visitor1 < RRB::Visitor
    def initialize
      @methods = []
    end
    attr_reader :methods

    def visit_method(namespace, node)
      @methods << RRB::Method.new(namespace, node)
    end
  end
  def test_name
    parser = RRB::Parser.new
    tree = parser.run File.open( "samples/parser_sample.rb", "r" )

    # test visit_class and visit_method
    visitor1 = Visitor1.new
    tree.accept( visitor1 )
    
    assert_equal(["TestClassA::TestClassB#method_4", "TestClassA#<=>", "TestClassA#method_1", "TestClassA#method_2", "TestClassA#method_3", "TestClassA#method_8", "TestClassA#method_9", "TestClassA::[sclass]#method_7"],
 visitor1.methods.map{|method| method.name})
    assert_equal(["method_4", "<=>", "method_1", "method_2", "method_3", "method_8", "method_9", "method_7"],
 visitor1.methods.map{|method| method.bare_name})

  end
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = RUNIT::TestSuite.new
    suite.add_test( TestNode.suite )
    suite.add_test( TestConstInfo.suite )
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestNode.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
