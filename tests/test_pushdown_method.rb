require 'runit/testcase'
require 'runit/cui/testrunner'

class TestScriptFile_PushdownMethod < RUNIT::TestCase
  
end

class TestScript_PushdownMethod < RUNIT::TestCase
  def test_pushdown_method?
    script = RRB::Script.new_from_filenames("samples/pushdown_method_sample.rb")
    assert_equals(true, script.pushdown_method?(RRB::NS['B'], 'x', RRB::NS['C']))
    assert_equals(false, script.pushdown_method?(RRB::NS['B'], 'y', RRB::NS['C']))
    assert_equals("B#y calls private function \"z\"\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['B'], 'z', RRB::NS['C']))
    assert_equals("Other function uses B#z\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['B'], 'w', RRB::NS['C']))
    assert_equals("C already has w\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['C'], 'w', RRB::NS['B']))    
    assert_equals("B is not the subclass of C\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['C'], 'asdf', RRB::NS['B']))    
    assert_equals("C doesn't have a function called asdf\n", script.error_message)

    assert_equals(true, script.pushdown_method?(RRB::NS['A'], 'a', RRB::NS['B']))
  end

  def test_pushdown_method
    script = RRB::Script.new_from_filenames("samples/pushdown_method_sample.rb")
    script.pushdown_method(RRB::NS['B'], 'x', RRB::NS['C'])
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pushdown_method_sample_after.rb' ).read,
                   dst )    
  end
end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestScriptFile_PushdownMethod )
  suite.add_test( TestScript_PushdownMethod )
  RUNIT::CUI::TestRunner.run(suite)
end

