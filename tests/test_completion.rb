require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/completion'

class TestScriptFile_Completion < RUNIT::TestCase

  INPUT = "
class A
  def method_1( arg1, arg2 )
    var = arg1 * arg2
  end
  def method_2
  end
end

class B < A
  def method_1( arg1, arg3 )
    print arg1
  end
end
class C < A
end
"
  
  def test_refactable_methods
    scriptfile = RRB::ScriptFile.new( StringIO.new( INPUT ), "/tmp/test.rb" )
    methods = scriptfile.refactable_methods.map do |method|
      [ method.fullname, method.local_vars ]
    end
    assert_equals( [[ 'A#method_1', Set[ 'arg1', 'arg2', 'var' ] ],
		    [ 'A#method_2', Set[]],
		    [ 'B#method_1', Set[ 'arg1', 'arg3' ] ]],
		  methods )
  end
  
end

class TestScript_Completion < RUNIT::TestCase

  INPUT = "\
/tmp/tmp/test1.rb\C-a
require 'test2'
class B < A
  def method_1( arg1, arg3 )
    print arg1
  end
end
\C-a/tmp/test2.rb\C-a
class A
  def method_1( arg1, arg2 )
    var = arg1 * arg2
  end
  def method_2
  end
end
\C-a-- END --\C-a
"
  def test_refactable_methods
    script = RRB::Script.new_from_io( StringIO.new( INPUT ) )
    methods = script.refactable_methods.map do |method|
      [ method.fullname, method.local_vars ]
    end
    assert_equals( [ [ 'B#method_1', Set[ 'arg1', 'arg3' ] ],
		      [ 'A#method_1', Set[ 'arg1', 'arg2', 'var' ] ],
		      [ 'A#method_2', Set[] ] ],
		  methods )
  end
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = RUNIT::TestSuite.new
    suite.add_test TestScriptFile_Completion.suite
    suite.add_test TestScript_Completion.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestScriptFile_Completion.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
