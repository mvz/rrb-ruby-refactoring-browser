require 'runit/testcase'
require 'runit/cui/testrunner'

class TestScriptFile_PullupMethod < RUNIT::TestCase
  
end

class TestScript_PullupMethod < RUNIT::TestCase
  def test_pullup_method?
    script = RRB::Script.new_from_filenames("samples/pullup_method_sample.rb")
    assert_equals(true, script.pullup_method?(RRB::NS['Derived'], 'bar', RRB::NS['Base']))
    assert_equals(false, script.pullup_method?(RRB::NS['Derived'], 'foo', RRB::NS['Base']))
    assert_equals("Derived#foo uses bar defined at Derived\n", script.error_message)    
    assert_equals(false, script.pullup_method?(RRB::NS['Derived'], 'hoge', RRB::NS['Base']))
    assert_equals("Derived doesn't have a function called hoge\n", script.error_message)    
    assert_equals(false, script.pullup_method?(RRB::NS['Base'], 'hoge', RRB::NS['Derived']))
    assert_equals("Derived is not the superclass of Base\n", script.error_message)    
    assert_equals(false, script.pullup_method?(RRB::NS['Derived'], 'asdf', RRB::NS['Base']))
    assert_equals("Base already has asdf\n", script.error_message)    

  end

  def test_pullup_method
    script = RRB::Script.new_from_filenames("samples/pullup_method_sample.rb")
    script.pullup_method(RRB::NS['Derived'], 'bar', RRB::NS['Base'])
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pullup_method_sample_after.rb' ).read,
                   dst )

  end  
end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestScriptFile_PullupMethod )
  suite.add_test( TestScript_PullupMethod )
  RUNIT::CUI::TestRunner.run(suite)
end

