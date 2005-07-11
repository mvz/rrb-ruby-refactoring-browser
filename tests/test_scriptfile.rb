require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/scriptfile'
require 'rrb/rename_local_var'
require 'rrb/rename_global_var'
require 'rrb/rename_method_all'
require 'rrb/extract_method'
require 'rrb/move_method'

class TestScriptFile < RUNIT::TestCase
  def test_mk_file_path
    assert_equals("/tmp/ruby/test.rb",
                  RRB::ScriptFile.new("", "ruby/test.rb").mk_file_path("/tmp"))
    assert_equals("/tmp//home/tester/ruby/test.rb",
                  RRB::ScriptFile.new("", "/home/tester/ruby/test.rb").mk_file_path("/tmp"))
    assert_equals("c:/tmp/c/ruby/test.rb",
                  RRB::ScriptFile.new("", "c:/ruby/test.rb").mk_file_path("c:/tmp"))
  end

  
  def test_s_new
    assert_raise(RRB::RRBError){
      RRB::ScriptFile.new("x = ;", "/home/ohai/test.rb")
    }
  end
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = TestScriptFile.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestScriptFile.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
