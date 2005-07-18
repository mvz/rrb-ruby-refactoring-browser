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
    assert_equals("", script.get_method_on_cursor('samples/parser_sample.rb', 69).name)
  end

  def test_get_class_on_cursor
    script = RRB::Script.new_from_filenames('samples/parser_sample.rb')
    assert_equals(RRB::NS[""], script.get_class_on_cursor('samples/parser_sample.rb', 1))
    assert_equals(RRB::NS["TestClassA"], script.get_class_on_cursor('samples/parser_sample.rb', 3))
    assert_equals(RRB::NS["TestClassA::TestClassB"], script.get_class_on_cursor('samples/parser_sample.rb', 28))

    assert_nil(script.get_class_on_cursor('samples/parser_sample.rb',2))
    assert_nil(script.get_class_on_cursor('samples/parser_sample.rb',27))
    assert_equals(RRB::NS["TestClassA"], script.get_class_on_cursor('samples/parser_sample.rb',27, false))
  end
end
