require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/pullup_method.rb'

class TestScriptFile_PullupMethod < RUNIT::TestCase
  
end

class TestScript_PullupMethod < RUNIT::TestCase
  def test_pullup_method?
    filename = "samples/pullup_method_sample.rb"

    script = RRB::Script.new_from_filenames(filename)
    assert_equals(true, script.pullup_method?(RRB::MN.new(RRB::NS['Derived'], 
                                                          'bar'),
                                              RRB::NS['Base'], filename, 2))
    assert_equals(false, script.pullup_method?(RRB::MN.new(RRB::NS['Derived'], 
                                                           'foo'),
                                               RRB::NS['Base'], filename, 2))
    assert_equals("Derived#foo uses Derived#bar\n", script.error_message)    
    assert_equals(false, script.pullup_method?(RRB::MN.new(RRB::NS['Derived'], 
                                                           'hoge'),
                                               RRB::NS['Base'], filename, 2))
    assert_equals("Derived#hoge is not defined\n", script.error_message)    
    assert_equals(false, script.pullup_method?(RRB::MN.new(RRB::NS['Base'], 
                                                           'hoge'),
                                               RRB::NS['Derived'], filename, 11))
    assert_equals("Derived is not the superclass of Base\n", script.error_message)    
    assert_equals(false, script.pullup_method?(RRB::MN.new(RRB::NS['Derived'], 'asdf'),
                                               RRB::NS['Base'], filename, 11))
    assert_equals("Base#asdf is already defined\n", script.error_message)    
  end

  def test_pullup_method
    filename = "samples/pullup_method_sample.rb"

    script = RRB::Script.new_from_filenames(filename)
    script.pullup_method(RRB::MN.new(RRB::NS['Derived'], 'bar'), 
                         RRB::NS['Base'], filename, 2)
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pullup_method_sample_after.rb' ).read,
                   dst )

    script = RRB::Script.new_from_filenames(filename)
    script.pullup_method(RRB::MN.new(RRB::NS['Derived::Derived2'], 'bar'), 
                         RRB::NS['Base'], filename, 2)
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pullup_method_sample_after2.rb' ).read,
                   dst )

  end  


  INPUT_STR = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base

end

class Base

end

class Derived < Base
  def hoge
  end
  def Derived.hoge
  end
end
\C-a/home/yuichi/work/rrb/private/test2.rb\C-a
class Base

end
\C-a/home/yuichi/work/rrb/private/test3.rb\C-a
class Derived < Base
end
\C-a-- END --\C-a
"
OUTPUT_STR1 = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base
  def hoge
  end

end

class Base

end

class Derived < Base
  def Derived.hoge
  end
end
\C-a-- END --\C-a
"
OUTPUT_STR2 = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base

end

class Base
  def hoge
  end

end

class Derived < Base
  def Derived.hoge
  end
end
\C-a-- END --\C-a
"
OUTPUT_STR3 = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base

end

class Base

end

class Derived < Base
  def Derived.hoge
  end
end
\C-a/home/yuichi/work/rrb/private/test2.rb\C-a
class Base
  def hoge
  end

end
\C-a-- END --\C-a
"
OUTPUT_STR4 = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base
  def Base.hoge
  end

end

class Base

end

class Derived < Base
  def hoge
  end
end
\C-a-- END --\C-a
"
  def test_pullup_method_plural_files?
    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    assert_equals(true,
                  script.pullup_method?(RRB::MN.new(RRB::NS['Derived'], 'hoge'),
                                        RRB::NS['Base'], '/home/yuichi/work/rrb/private/test.rb', 3))
    assert_equals(true,
                  script.pullup_method?(RRB::CMN.new(RRB::NS['Derived'], 'hoge'),
                                        RRB::NS['Base'], '/home/yuichi/work/rrb/private/test.rb', 3))

    assert_equals(false,
                  script.pullup_method?(RRB::MN.new(RRB::NS['Derived'], 'hoge'),
                                        RRB::NS['Base'], '/home/yuichi/work/rrb/private/test3.rb', 3))
    assert_equals("No definition of Base in /home/yuichi/work/rrb/private/test3.rb\n", script.error_message)
    assert_equals(false,
                  script.pullup_method?(RRB::CMN.new(RRB::NS['Derived'], 'hoge'),
                                        RRB::NS['Base'], '/home/yuichi/work/rrb/private/test3.rb', 3))
    assert_equals("No definition of Base in /home/yuichi/work/rrb/private/test3.rb\n", script.error_message)

    assert_equals(false,
                  script.pullup_method?(RRB::MN.new(RRB::NS['Derived'], 'hoge'),
                                        RRB::NS['Base'], '/home/yuichi/work/rrb/private/test.rb', 9))
    assert_equals("Specify which definition to pull up method to\n", script.error_message)
    assert_equals(false,
                  script.pullup_method?(RRB::CMN.new(RRB::NS['Derived'], 'hoge'),
                                        RRB::NS['Base'], '/home/yuichi/work/rrb/private/test.rb', 9))
    assert_equals("Specify which definition to pull up method to\n", script.error_message)

  end

  def test_pullup_method_plural_files
    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pullup_method(RRB::MN.new(RRB::NS['Derived'], 'hoge'),
                         RRB::NS['Base'], 
                         '/home/yuichi/work/rrb/private/test.rb', 3)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR1, dst)

    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pullup_method(RRB::MN.new(RRB::NS['Derived'], 'hoge'),
                         RRB::NS['Base'], 
                         '/home/yuichi/work/rrb/private/test.rb', 7)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR2, dst)

    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pullup_method(RRB::MN.new(RRB::NS['Derived'], 'hoge'), RRB::NS['Base'], 
                           '/home/yuichi/work/rrb/private/test2.rb', 3)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR3, dst)
    
    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pullup_method(RRB::CMN.new(RRB::NS['Derived'], 'hoge'),
                         RRB::NS['Base'], 
                           '/home/yuichi/work/rrb/private/test.rb', 3)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR4, dst)
  end

end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestScriptFile_PullupMethod.suite )
  suite.add_test( TestScript_PullupMethod.suite )
  RUNIT::CUI::TestRunner.run(suite)
end

