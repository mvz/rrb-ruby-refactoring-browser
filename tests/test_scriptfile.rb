require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/scriptfile'
require 'rrb/rename_local_var'
require 'rrb/rename_global_var'
require 'rrb/rename_method_all'
require 'rrb/extract_method'
require 'rrb/move_method'

class TestScriptFile < RUNIT::TestCase

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
