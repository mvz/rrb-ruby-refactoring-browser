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
      @methods << [ namespace.top.name, method_node.name ]
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
      if node.name_id == 'toplevel' then
	@nodes << [ 'toplevel']
      else
	@nodes << namespace.map{|i| i.name}.push( node.name )
      end
    end

    
  end
  
  def test_accept

    parser = RRB::Parser.new
    tree = parser.run File.open( TEST_SCRIPT_NAME, "r" )

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
		  ],
		  visitor1.methods.sort )

    visitor2 = Visitor2.new
    tree.accept( visitor2 )
    assert_equals( [ [[], 'TestClassA'],
		    [['TestClassA'],'TestClassB'],
		    [['TestClassA','TestClassB'],'TestClassC'],
		    [['TestClassA'],'TestModuleA'],
		    [['TestClassA','TestModuleA'],'TestModuleB'],
		  ],
		  visitor2.classes )

    visitor3 = Visitor3.new
    tree.accept( visitor3 )
    assert_equals( [ 'TestClassA' ], visitor3.top_classes )

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
		    ['TestClassA','TestModuleA','TestModuleB','method_5']
		  ].sort, visitor4.nodes.sort )
  end

end

if $0 == __FILE__
  if ARGV.size == 0
    suite = TestNode.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestNode.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
