require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/pushdown_method'

class TestScriptFile_PushdownMethod < RUNIT::TestCase
  
end

class TestScript_PushdownMethod < RUNIT::TestCase
  def test_pushdown_method?
    filename = "samples/pushdown_method_sample.rb"

    script = RRB::Script.new_from_filenames(filename)
    assert_equals(true, script.pushdown_method?(RRB::MN.new(RRB::NS['B'], 'x'),
                                                RRB::NS['C'], filename, 23))
    assert_equals(false, script.pushdown_method?(RRB::MN.new(RRB::NS['B'],'z'),
                                                 RRB::NS['C'], filename, 23))
    assert_equals("B calls B#z", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::MN.new(RRB::NS['B'], 'w'),
                                                 RRB::NS['C'], filename, 23))
    assert_equals("B#w: already defined at C\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::MN.new(RRB::NS['C'],'w'),
                                                 RRB::NS['B'], filename, 7))    
    assert_equals("B is not the subclass of C\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::MN.new(RRB::NS['C'],'asdf'),
                                                 RRB::NS['B'], filename, 7))    
    assert_equals("C#asdf: no definition in C\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::MN.new(RRB::NS['A'], 'a'),
                                                 RRB::NS['C'], filename, 23))
    assert_equals("B calls A#a", script.error_message)

    assert_equals(true, script.pushdown_method?(RRB::MN.new(RRB::NS['A'], 'a'),
                                                RRB::NS['B'], filename, 7))
  end

  def test_pushdown_method
    filename = "samples/pushdown_method_sample.rb"
    lineno = 10
    script = RRB::Script.new_from_filenames(filename)
    script.pushdown_method(RRB::MN.new(RRB::NS['B'], 'x'),
                           RRB::NS['C'], filename, lineno)
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pushdown_method_sample_after.rb' ).read,
                   dst )    
    script = RRB::Script.new_from_filenames(filename)
    script.pushdown_method(RRB::MN.new(RRB::NS['B'], 'x'),
                           RRB::NS['C::D'], filename, lineno)
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pushdown_method_sample_after2.rb' ).read,
                   dst )    
  end

  INPUT_STR = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base
  def Base.hoge
  end
  def hoge
  end
end

class Derived < Base

end

class Derived < Base

end
\C-a/home/yuichi/work/rrb/private/test2.rb\C-a
class Derived < Base

end
\C-a/home/yuichi/work/rrb/private/test3.rb\C-a
class Base

end
\C-a-- END --\C-a
"
OUTPUT_STR1 = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base
  def Base.hoge
  end
end

class Derived < Base
  def hoge
  end

end

class Derived < Base

end
\C-a-- END --\C-a
"
OUTPUT_STR2 = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base
  def Base.hoge
  end
end

class Derived < Base

end

class Derived < Base
  def hoge
  end

end
\C-a-- END --\C-a
"
OUTPUT_STR3 = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base
  def Base.hoge
  end
end

class Derived < Base

end

class Derived < Base

end
\C-a/home/yuichi/work/rrb/private/test2.rb\C-a
class Derived < Base
  def hoge
  end

end
\C-a-- END --\C-a
"
OUTPUT_STR4 = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base
  def hoge
  end
end

class Derived < Base
  def Derived.hoge
  end

end

class Derived < Base

end
\C-a-- END --\C-a
"
  def test_pushdown_method_plural_files?
    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    assert_equals(true,
                  script.pushdown_method?(RRB::MN.new(RRB::NS['Base'], 'hoge'),
                                          RRB::NS['Derived'], '/home/yuichi/work/rrb/private/test.rb', 10))

    assert_equals(true,
                  script.pushdown_method?(RRB::CMN.new(RRB::NS['Base'], 'hoge'),
                                          RRB::NS['Derived'], '/home/yuichi/work/rrb/private/test.rb', 10))

    assert_equals(false,
                  script.pushdown_method?(RRB::MN.new(RRB::NS['Base'], 'hoge'),
                                          RRB::NS['Derived'], '/home/yuichi/work/rrb/private/test3.rb', 5))
    assert_equals("No definition of Derived in /home/yuichi/work/rrb/private/test3.rb\n", script.error_message)

    assert_equals(false,
                  script.pushdown_method?(RRB::CMN.new(RRB::NS['Base'], 'hoge'),
                                          RRB::NS['Derived'], '/home/yuichi/work/rrb/private/test3.rb', 5))
    assert_equals("No definition of Derived in /home/yuichi/work/rrb/private/test3.rb\n", script.error_message)

    assert_equals(false,
                  script.pushdown_method?(RRB::MN.new(RRB::NS['Base'], 'hoge'),
                                          RRB::NS['Derived'], '/home/yuichi/work/rrb/private/test.rb', 8))
    assert_equals("Specify which definition to push down method to\n", script.error_message)

    assert_equals(false,
                  script.pushdown_method?(RRB::CMN.new(RRB::NS['Base'], 'hoge'),
                                          RRB::NS['Derived'], '/home/yuichi/work/rrb/private/test.rb', 8))
    assert_equals("Specify which definition to push down method to\n", script.error_message)

  end


  def test_pushdown_method_plural_files
    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pushdown_method(RRB::MN.new(RRB::NS['Base'], 'hoge'),
                           RRB::NS['Derived'], 
                           '/home/yuichi/work/rrb/private/test.rb', 10)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR1, dst)

    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pushdown_method(RRB::MN.new(RRB::NS['Base'], 'hoge'),
                           RRB::NS['Derived'], 
                           '/home/yuichi/work/rrb/private/test.rb', 13)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR2, dst)

    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pushdown_method(RRB::MN.new(RRB::NS['Base'], 'hoge'),
                           RRB::NS['Derived'], 
                           '/home/yuichi/work/rrb/private/test2.rb', 5)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR3, dst)

    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pushdown_method(RRB::CMN.new(RRB::NS['Base'], 'hoge'),
                           RRB::NS['Derived'], 
                           '/home/yuichi/work/rrb/private/test.rb', 10)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR4, dst)
    
  end
end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestScriptFile_PushdownMethod.suite )
  suite.add_test( TestScript_PushdownMethod.suite )
  RUNIT::CUI::TestRunner.run(suite)
end

