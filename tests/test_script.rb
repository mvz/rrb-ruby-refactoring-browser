require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/script.rb'
require 'rrb/rename_local_var'
require 'rrb/rename_instance_var'
require 'rrb/rename_class_var'
require 'rrb/rename_method_all'
require 'rrb/pullup_method'
require 'rrb/pushdown_method'
require 'rrb/rename_global_var'

class TestScript < RUNIT::TestCase

TEST_CLASS_A_METHOD_1 = 
'  def method_1( a, b )
    c = 3; @x = 2
    call( 3, 2 )
    a.each{|x| print x}
    yield b+c
  end
'

  def test_get_string_of_method
    script = RRB::Script.new_from_filenames('samples/parser_sample.rb')
    assert_equals(TEST_CLASS_A_METHOD_1, 
                  script.get_string_of_method(RRB::MN.new(RRB::NS.new("TestClassA"), 
                                                          'method_1')))
  end

  def test_get_class_on_region
    script = RRB::Script.new_from_filenames('samples/parser_sample.rb')
    assert_equals('TestClassA', script.get_class_on_region('samples/parser_sample.rb', 3..6).name)
    assert_equals(true, script.get_class_on_region('samples/parser_sample.rbrb', 3..6).nil?)
    assert_equals(true, script.get_class_on_region('samples/parser_sample.rb', 26..28).nil?)
    assert_equals('', script.get_class_on_region('samples/parser_sample.rb', 1..1).name)
  end

  def test_get_method_on_region
    script = RRB::Script.new_from_filenames('samples/parser_sample.rb')
    assert_equals('TestClassA#method_1', script.get_method_on_region('samples/parser_sample.rb', 8..11).name)
    assert_equals(true, script.get_method_on_region('samples/parser_sample.rbrb', 8..11).nil?)
    assert_equals(true, script.get_method_on_region('samples/parser_sample.rb', 7..11).nil?)
    assert_equals("TestClassA.method_5", script.get_method_on_region('samples/parser_sample.rb', 38..38).name)
  end


  TEST_DUMP_INPUT = "\
/home/ohai/ruby/test/file1.rb\C-a

# comment

class Rename
  def method_1( x, y )
    z = 3
    z.upto(6) do |i|
      print i*3, \"\\n\"
    end
    print z**4, z**5
  end
end
\C-a/home/ohai/ruby/test/file2.rb\C-a
class Rename
  def method_2( x, y)
    x**2 + y**2
  end
end
\C-a-- END --\C-a
"

  def test_dump
    script = RRB::Script.new_from_io( StringIO.new( TEST_DUMP_INPUT ) )
    info = script.get_dumped_info
    assert_equals( "class", info["Rename"].type )
    assert_equals( ["method_1","method_2"].sort,
                   info["Rename"].public_method_names.sort )
  end
  
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = TestScript.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestScript.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
