require 'runit/testcase'
require 'runit/cui/testrunner'

class TestScriptFile_RenameClassVar < RUNIT::TestCase
  
end

class TestScript_RenameClassVar < RUNIT::TestCase
  def test_rename_class_var
    script = RRB::Script.new_from_filenames('samples/rename_class_var_sample.rb')
    script.rename_class_var(RRB::NS['X::A'], '@@a', '@@f')
    dst = ''
    script.result_to_io(dst)
    assert_equals(File.open('samples/rename_class_var_sample_after.rb').read, dst)
    script = RRB::Script.new_from_filenames('samples/rename_class_var_sample.rb')
    script.rename_class_var(RRB::NS['X::B'], '@@a', '@@f')
    dst = ''
    script.result_to_io(dst)
    assert_equals(File.open('samples/rename_class_var_sample_after.rb').read, dst)
                    
  end


  def test_rename_class_var?
    script = RRB::Script.new_from_filenames('samples/rename_class_var_sample.rb')
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@b'))
    assert_equals("X::A already has @@b\n", script.error_message)
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@c'))
    assert_equals("X::B already has @@c\n", script.error_message)
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@d'))
    assert_equals("X::C already has @@d\n", script.error_message)
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@e'))
    assert_equals("X::D already has @@e\n", script.error_message)
    assert_equals(false, script.rename_class_var?(RRB::NS['X::A'], '@@a', 'fff'))
    assert_equals("fff is not a valid name for class variables\n", script.error_message)    
  end

  
end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestScriptFile_RenameClassVar )
  suite.add_test( TestScript_RenameClassVar )
  RUNIT::CUI::TestRunner.run(suite)
end

