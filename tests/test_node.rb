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

    def visit_method( namespace, method_node )
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
