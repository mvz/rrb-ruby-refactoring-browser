require 'runit/testcase'
require 'runit/cui/testrunner'

class TestScriptFile_ExtractMethod < RUNIT::TestCase

  def test_extract_method
    File.open( 'samples/extract_method_sample.rb', 'r' ) do |file|
      script_file = RRB::ScriptFile.new( file, file.path )
      script_file.extract_method('bar', 11, 14)
      assert_equals( File.open( 'samples/extract_method_sample_after.rb' ).read,
                    script_file.new_script )
    end
  end

  def test_get_emethod_namespace
    File.open( 'samples/extract_method_sample.rb', 'r' ) do |file|
      script_file = RRB::ScriptFile.new( file, file.path )
      assert_equals( RRB::NS["B"],
                    script_file.get_emethod_namespace( 11, 14 ).normal )
    end
  end

end

class TestScript_ExtractMethod < RUNIT::TestCase
  

  def test_extract_method?
    str_filename = 'samples/extract_method_sample.rb'
    script = RRB::Script.new_from_filenames(str_filename)
    
    assert_equals(true, script.extract_method?(str_filename, 'bar', 11, 14))
    assert_equals(false, script.extract_method?(str_filename, 'hogehoge', 11, 14))
    assert_equals(false, script.extract_method?(str_filename, 'foo', 11, 14))
    assert_equals(false, script.extract_method?(str_filename, 'bar', 8, 9))
    assert_equals(true, script.extract_method?(str_filename, 'hoge', 21, 22))
  end

end

if $0 == __FILE__
  if ARGV.size == 0
    suite = RUNIT::TestSuite.new
    suite.add_test(TestScript_ExtractMethod.suite)
    suite.add_test(TestScriptFile_ExtractMethod.suite)
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestScript_ExtractMethod.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end