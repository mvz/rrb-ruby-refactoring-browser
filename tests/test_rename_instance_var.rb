require 'runit/testcase'
require 'runit/cui/testrunner'

class TestScriptFile_RenameInstanceVar < RUNIT::TestCase
  
end

class TestScript_RenameInstanceVar < RUNIT::TestCase

  def test_rename_instance_var
    script = RRB::Script.new_from_filenames('samples/rename_instance_var_sample.rb','samples/rename_instance_var_sample2.rb')
    script.rename_instance_var(RRB::NS.new('X::A'), '@a', '@f')
    dst = ''
    script.result_to_io(dst)
    assert_equals(File.open('samples/rename_instance_var_sample_after.rb').read, dst)
    script = RRB::Script.new_from_filenames('samples/rename_instance_var_sample.rb','samples/rename_instance_var_sample2.rb')
    script.rename_instance_var(RRB::NS.new(['X', 'B']), '@a', '@f')
    dst = ''
    script.result_to_io(dst)
    assert_equals(File.open('samples/rename_instance_var_sample_after.rb').read, dst)
                    
  end


  def test_rename_instance_var?
    script = RRB::Script.new_from_filenames('samples/rename_instance_var_sample.rb','samples/rename_instance_var_sample2.rb')
    assert_equals(true, script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@f'))
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@b'))
    assert_equals("@b: already used by X::A", script.error_message)
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@c'))
    assert_equals("@c: already used by X::B", script.error_message)
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@d'))
    assert_equals("@d: already used by X::C", script.error_message)
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', 'ee'))
    assert_equals("ee: not a valid name for instance variables",
                  script.error_message)

  end
  
end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestScriptFile_RenameInstanceVar )
  suite.add_test( TestScript_RenameInstanceVar )
  RUNIT::CUI::TestRunner.run(suite)
end

