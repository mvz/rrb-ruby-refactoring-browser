require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/remove_parameter'

class TestScript_RemoveParameter < RUNIT::TestCase

  def test_get_parameter_index
    script = RRB::Script.new_from_filenames('samples/remove_parameter_sample.rb')
    assert_equals(0, script.get_parameter_index(RRB::NS.new('Derived'),
                                                'target_function', 'heke'))
    assert_equals(0, script.get_parameter_index(RRB::NS.new('Derived'),
                                                'using_parameter_function',
                                                'heke'))
    assert_equals(1, script.get_parameter_index(RRB::NS.new('Derived'),
                                                'using_parameter_function',
                                                'doga'))
  end

  def test_remove_parameter?
    script = RRB::Script.new_from_filenames('samples/remove_parameter_sample.rb')
    assert_equals(true,
                  script.remove_parameter?(RRB::NS.new('Derived'), 
                                               'target_function', 'heke'))
    assert_equals(false,
                  script.remove_parameter?(RRB::NS.new('Derived'), 
                                               'target_function', 'hoge'))
    assert_equals("hoge: no such parameter\n", script.error_message)
    assert_equals(false,
                  script.remove_parameter?(RRB::NS.new('Derived'), 
                                           'using_parameter_function',
                                           'heke'))
    assert_equals("heke is used\n",
                  script.error_message)

    assert_equals(false,
                  script.remove_parameter?(RRB::NS.new('Derived'), 
                                               'base_function', 'heke'))
    assert_equals("base_function isn't defined at Derived\n",
                  script.error_message)

  end

  def test_remove_parameter
    script = RRB::Script.new_from_filenames('samples/remove_parameter_sample.rb')
    script.remove_parameter(RRB::NS.new('Derived'), 'target_function', 'heke')
    result = ''
    script.result_to_io( result )
    assert_equals( File.open('samples/remove_parameter_sample_after.rb').read,
                   result )
  end
  
end

class TestScriptFile_RemoveParameter < RUNIT::TestCase

end

if $0 == __FILE__
  if ARGV.size == 0
    suite = RUNIT::TestSuite.new
    suite.add_test( TestScriptFile_RemoveParameter.suite )
    suite.add_test( TestScript_RemoveParameter.suite )
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestScript_RemoveParameter.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
