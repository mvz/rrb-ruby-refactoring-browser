require 'runit/testcase'
require 'runit/cui/testrunner'

class TestScriptFile_PushdownMethod < RUNIT::TestCase
  
end

class TestScript_PushdownMethod < RUNIT::TestCase
  def test_pushdown_method?
    filename = "samples/pushdown_method_sample.rb"

    script = RRB::Script.new_from_filenames(filename)
    assert_equals(true, script.pushdown_method?(RRB::NS['B'], 
                                                RRB::MN.new('x', true),
                                                RRB::NS['C'], filename, 23))
    assert_equals(false, script.pushdown_method?(RRB::NS['B'],
                                                 RRB::MN.new('y', true),
                                                 RRB::NS['C'], filename, 23))
    assert_equals("B#y calls private function \"z\"\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['B'],
                                                 RRB::MN.new('z', true),
                                                 RRB::NS['C'], filename, 23))
    assert_equals("B#z: called by other function\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['B'], 
                                                 RRB::MN.new('w', true),
                                                 RRB::NS['C'], filename, 23))
    assert_equals("w: already defined at C\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['C'],
                                                 RRB::MN.new('w', true),
                                                 RRB::NS['B'], filename, 7))    
    assert_equals("B is not the subclass of C\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['C'],
                                                 RRB::MN.new('asdf', true),
                                                 RRB::NS['B'], filename, 7))    
    assert_equals("asdf: no definition in C\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['A'], 
						 RRB::MN.new('a', true),
                                                 RRB::NS['C'], filename, 23))
    assert_equals("Other subclass calls A#a\n", script.error_message)

    assert_equals(true, script.pushdown_method?(RRB::NS['A'], 
                                                RRB::MN.new('a',true),
                                                RRB::NS['B'], filename, 7))
  end

  def test_pushdown_method
    filename = "samples/pushdown_method_sample.rb"
    lineno = 10
    script = RRB::Script.new_from_filenames(filename)
    script.pushdown_method(RRB::NS['B'], 
                           RRB::MN.new('x', true),
                           RRB::NS['C'], filename, lineno)
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pushdown_method_sample_after.rb' ).read,
                   dst )    
    script = RRB::Script.new_from_filenames(filename)
    script.pushdown_method(RRB::NS['B'], 
                           RRB::MN.new('x', true),
                           RRB::NS['C::D'], filename, lineno)
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pushdown_method_sample_after2.rb' ).read,
                   dst )    
  end

  INPUT_STR = "\
/home/yuichi/work/rrb/private/test.rb\C-a
class Base
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
  def test_pushdown_method_plural_files?
    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    assert_equals(true,
                  script.pushdown_method?(RRB::NS['Base'], 
                                          RRB::MN.new('hoge', true),
                                          RRB::NS['Derived'], '/home/yuichi/work/rrb/private/test.rb', 8))

    assert_equals(false,
                  script.pushdown_method?(RRB::NS['Base'], 
                                          RRB::MN.new('hoge', true),
                                          RRB::NS['Derived'], '/home/yuichi/work/rrb/private/test3.rb', 3))
    assert_equals("No definition of Derived in /home/yuichi/work/rrb/private/test3.rb\n", script.error_message)
    assert_equals(false,
                  script.pushdown_method?(RRB::NS['Base'], 
                                          RRB::MN.new('hoge', true),
                                          RRB::NS['Derived'], '/home/yuichi/work/rrb/private/test.rb', 6))
    assert_equals("Specify which definition to push down method to\n", script.error_message)

  end


  def test_pushdown_method_plural_files
    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pushdown_method(RRB::NS['Base'], 
                           RRB::MN.new('hoge', true),
                           RRB::NS['Derived'], 
                           '/home/yuichi/work/rrb/private/test.rb', 8)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR1, dst)

    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pushdown_method(RRB::NS['Base'], 
                           RRB::MN.new('hoge', true),
                           RRB::NS['Derived'], 
                           '/home/yuichi/work/rrb/private/test.rb', 11)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR2, dst)

    script = RRB::Script.new_from_io( StringIO.new(INPUT_STR ) )
    script.pushdown_method(RRB::NS['Base'], 
                           RRB::MN.new('hoge', true),
                           RRB::NS['Derived'], 
                           '/home/yuichi/work/rrb/private/test2.rb', 3)
    dst = ''
    script.result_to_io(dst)
    assert_equals(OUTPUT_STR3, dst)
    
  end
end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestScriptFile_PushdownMethod )
  suite.add_test( TestScript_PushdownMethod )
  RUNIT::CUI::TestRunner.run(suite)
end

