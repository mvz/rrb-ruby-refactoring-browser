require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/scriptfile'
require 'rrb/rename_local_var'
require 'rrb/rename_method_all'

class TestScriptFile < RUNIT::TestCase

  def test_rename_local_var

    File.open( 'samples/rename_var_sample.rb', 'r' ) do |file|

      script_file = RRB::ScriptFile.new( file, file.path )
      script_file.rename_local_var( ['Rename'], 'method_1', 'z', 'bb' )
      assert_equals( File.open( 'samples/rename_var_sample_after.rb' ).read,
		    script_file.new_script )
    end
    
  end

  def test_rename_local_var?
    
    File.open( 'samples/rename_var_sample.rb', 'r' ) do |file|
      script_file = RRB::ScriptFile.new( file, file.path )
      assert_equals( true, script_file.rename_local_var?( ['Rename'],
							 'method_1',
							 'z', 'bb' ) )
      # collision with other variable
      assert_equals( false, script_file.rename_local_var?( ['Rename'],
							 'method_1',
							 'z', 'x' ) )
      assert_equals( false, script_file.rename_local_var?( ['Rename'],
							 'method_1',
							  'z', 'i' ) )
      # invalid identifier 
      assert_equals( false, script_file.rename_local_var?( ['Rename'],
							 'method_1',
							  'z', 'Z' ) )
      assert_equals( false, script_file.rename_local_var?( ['Rename'],
							 'method_1',
							  'z', 'print' ) )
      assert_equals( false, script_file.rename_local_var?( ['Rename'],
							 'method_1',
							  'z', 'super' ) )
    end
    
  end

  
  def test_rename_method_all

    File.open('samples/rename_method_sample.rb') do |file|
      script_file = RRB::ScriptFile.new( file, file.path )
      script_file.rename_method_all( 'foo', 'feefoo' )
      assert_equals( File.open( 'samples/rename_method_sample_after.rb' ).read,
		    script_file.new_script )
    end
    
  end

  def test_rename_method_all?
    
    File.open('samples/rename_method_sample.rb') do |file|
      script_file = RRB::ScriptFile.new( file, file.path )
      assert_equals( true, script_file.rename_method_all?( 'foo', 'feefoo' ) )
      assert_equals( true, script_file.rename_method_all?( 'foo', 'foo?' ) )
      # collision with local variable
      assert_equals( false, script_file.rename_method_all?( 'baz', 'hek' ) )
      # invalid method name 
      assert_equals( false, script_file.rename_method_all?( 'foo', 'Foo' ) )
      assert_equals( false, script_file.rename_method_all?( 'foo', 'foo=' ) )
      assert_equals( false, script_file.rename_method_all?( 'foo', 'when' ) )
      assert_equals( false, script_file.rename_method_all?( 'foo', 'foo?fee' ))
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
