require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/scriptfile'
require 'rrb/rename_local_var'
require 'rrb/rename_global_var'
require 'rrb/rename_method_all'
require 'rrb/extract_method'
require 'rrb/move_method'

class TestScriptFile < RUNIT::TestCase


  

  def move_method
    File.open( 'samples/move_method_sample.rb', 'r' ) do |file|
      script_file = RRB::ScriptFile.new( file, file.path )
      script_file.move_method('foo', 'X::B', 'X::A')
      assert_equals( File.open( 'samples/move_method_sample_after.rb' ).read,		    script_file.new_script )
    end
  end
  def test_move_method?
    File.open( 'samples/move_method_sample.rb', 'r' ) do |file|
      script_file = RRB::ScriptFile.new( file.read, file.path )
      assert_equals(true, script_file.move_method?('foo', RRB::NS.new('X::B'), RRB::NS.new('X::A')))
      assert_equals(false, script_file.move_method?('bar', RRB::NS.new('X::B'), RRB::NS.new('X::A')))
      assert_equals(false, script_file.move_method?('foos', RRB::NS.new('X::B'), RRB::NS.new('X::A')))
      assert_equals(false, script_file.move_method?('bar', RRB::NS.new('X::A'), RRB::NS.new('X::B')))
    end    

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
