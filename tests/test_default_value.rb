require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/default_value.rb'

class TestDefaultValue < RUNIT::TestCase
  def test_get_method_on_cursor
    script = RRB::Script.new_from_filenames('samples/parser_sample.rb')
    assert_equals('TestClassA#method_1', script.get_method_on_cursor('samples/parser_sample.rb', 9).name)
    assert_equals('TestClassA#method_2', script.get_method_on_cursor('samples/parser_sample.rb', 15).name)
    assert_equals('TestClassA::[sclass]#method_7', script.get_method_on_cursor('samples/parser_sample.rb', 46).name)
    assert_equals('TestClassA#method_8', script.get_method_on_cursor('samples/parser_sample.rb', 53).name)
    assert_equals(nil, script.get_method_on_cursor('samples/parser_sample.rb', 69))
  end

  def test_get_class_on_cursor
    script = RRB::Script.new_from_filenames('samples/parser_sample.rb')
    assert_equals("", script.get_class_on_cursor('samples/parser_sample.rb', 1).name)
    assert_equals("TestClassA", script.get_class_on_cursor('samples/parser_sample.rb', 3).name)
    assert_equals("TestClassA::TestClassB", script.get_class_on_cursor('samples/parser_sample.rb', 28).name)
  end
end
