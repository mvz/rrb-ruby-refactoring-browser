require 'runit/testcase'
require 'runit/cui/testrunner'
require 'rrb/common_visitor'
require 'rrb/extract_method'

  INPUT_STR = "\
/home/oxy/work/rrb/private/test.rb\C-a
class X
  class A
    def A.hoge
      a = 1
      b = 1
      c = a * b + 1
    end
    def hoge
      a = 1
      b = 1
      c = a * b
    end
  end
end
\C-a-- END --\C-a
"
OUTPUT_STR1 = "/home/oxy/work/rrb/private/test.rb\C-a
class X
  class A
    def self.fuga()
      a = 1
      b = 1
      return a, b
    end
    def A.hoge
      a, b = fuga()
      c = a * b + 1
    end
    def hoge
      a = 1
      b = 1
      c = a * b
    end
  end
end
\C-a-- END --\C-a
"

OUTPUT_STR2 = "\
/home/oxy/work/rrb/private/test.rb\C-a
class X
  class A
    def A.hoge
      a = 1
      b = 1
      c = a * b + 1
    end
    def fuga(a)
      b = 1
      c = a * b
    end
    def hoge
      a = 1
      fuga(a)
    end
  end
end
\C-a-- END --\C-a
"


class TestScriptFile_ExtractMethod < RUNIT::TestCase

  def test_extract_method
    File.open( 'samples/extract_method_sample.rb', 'r' ) do |file|
      script_file = RRB::ScriptFile.new( file.read, file.path )
      script_file.extract_method('fuga', 11, 14)
      assert_equals( File.open( 'samples/extract_method_sample_after.rb' ).read,
                    script_file.new_script )
    end
  end

  def test_get_emethod_namespace
    File.open( 'samples/extract_method_sample.rb', 'r' ) do |file|
      script_file = RRB::ScriptFile.new( file.read, file.path )
      assert_equals( RRB::NS["B"],
                    script_file.get_class_on_region( 11..14 ) )
    end
  end

end

class TestScript_ExtractMethod < RUNIT::TestCase
  def test_extract_method
    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.extract_method("/home/oxy/work/rrb/private/test.rb", 'fuga', 5, 6)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR1, dst)

    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.extract_method("/home/oxy/work/rrb/private/test.rb", 'fuga', 11, 12)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR2, dst)

  end

  def test_extract_method?
    str_filename = 'samples/extract_method_sample.rb'
    
    script = RRB::Script.new_from_filenames(str_filename)

    assert_equals(true, script.extract_method?(str_filename, 'fuga', 11, 14))
    assert_equals(false, script.extract_method?(str_filename, 'fuga', 8, 9))
    assert_equals("please select statements", script.error_message)
    assert_equals(false, script.extract_method?(str_filename, 'hogehoge', 11, 14))
    assert_equals("hogehoge: already defined at B", script.error_message)
    assert_equals(false, script.extract_method?(str_filename, 'foo', 11, 14))
    assert_equals("foo: already defined at B", script.error_message)
    assert_equals(false, script.extract_method?(str_filename, 'bar', 8, 9))
    assert_equals("please select statements", script.error_message)
    assert_equals(false, script.extract_method?(str_filename, '@asdf', 8, 9))
    assert_equals("@asdf is not a valid name for methods",
                  script.error_message)
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
