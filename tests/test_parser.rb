require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/parser.rb'



class TestParser < RUNIT::TestCase

  TEST_SCRIPT_NAME = 'samples/parser_sample.rb'
  
  def test_parse
    parser = RRB::Parser.new
    parsed_info = parser.run File.open( TEST_SCRIPT_NAME, "r" )

    class_info = parsed_info.class_info("TestClassA")

    # test method_calls
    method_1_info = class_info.method_info("method_1")
    assert_equals( %w(each) ,
		  method_1_info.method_calls.map{|info| info.name} )
    assert_equals( %w(call print),
		  method_1_info.fcalls.map{|info| info.name} )
    
    method_2_info = class_info.method_info("method_2")
    assert_equals( [],
		  method_2_info.method_calls.map{|info| info.name} )
    assert_equals( %w(call! call p),
		  method_2_info.fcalls.map{|info| info.name} )

    method_3_info = class_info.method_info("method_3")
    assert_equals( [],
		  method_3_info.method_calls.map{|info| info.name} )
    assert_equals( %w(ConstNameCall),
		  method_3_info.fcalls.map{|info| info.name} )

    # test local_vars
    assert_equals( %w(a b c a x x b c),
		  method_1_info.local_vars.map{|info| info.name} )
    assert_equals( %w(c rest block call call),
		  method_2_info.local_vars.map{|info| info.name} )

    # test singleton method
    method_5_info = class_info.class_method_defs.find{|x| x.name == "method_5"}
    assert_equals( %w(x y x y),
		  method_5_info.local_vars.map{|info| info.name} )

    method_6_info = class_info.singleton_method_defs.find{|x|
      x.name == "method_6"
    }
    assert_not_nil( method_6_info )

    # test singleton class
    method_7_info = class_info.singleton_class_defs[0].method_info("method_7")
    assert_not_nil( method_7_info )
    
  end
  
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = TestParser.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestParser.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
