require 'runit/testcase'
require 'runit/cui/testrunner'

class TestScriptFile_RenameGlobalVar < RUNIT::TestCase
  
end

class TestScript_RenameGlobalVar < RUNIT::TestCase
  def test_rename_global_var
    script = RRB::Script.new_from_filenames("samples/rename_global_var_sample.rb")   
    script.rename_global_var('$x', '$y' )
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/rename_global_var_sample_after.rb' ).read, dst)
  end

  def test_rename_global_var?
    script = RRB::Script.new_from_filenames("samples/rename_global_var_sample.rb")   
    assert_equals( true, script.rename_global_var?('$x', '$y'))
    assert_equals( false, script.rename_global_var?('$x', '$z'))
    assert_equals("$z: already used\n", 
                  script.error_message)
    assert_equals( false, script.rename_global_var?('$x', 'x'))
    assert_equals("x: not a valid name for global variables\n", 
                  script.error_message)
    assert_equals( false, script.rename_global_var?('$x', '@x'))
    assert_equals("@x: not a valid name for global variables\n", 
                  script.error_message)
    assert_equals( false, script.rename_global_var?('$x', '@@x'))
    assert_equals("@@x: not a valid name for global variables\n", 
                  script.error_message)
    assert_equals( false, script.rename_global_var?('$x', 'print'))
    assert_equals("print: not a valid name for global variables\n", 
                  script.error_message)
  end
end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestScriptFile_RenameGlobalVar )
  suite.add_test( TestScript_RenameGlobalVar )
  RUNIT::CUI::TestRunner.run(suite)
end

