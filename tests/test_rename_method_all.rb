require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/rename_method_all'

class TestScriptFile_RenameMethodAll < RUNIT::TestCase

  INPUT_STR = <<EOS
class A

  def initialize
    @i = 0
  end
  
  def foo( x )
    @i += x
  end

  def baz
    @i
  end
  
end

class B < A

  def foo( x )
    @i += x*2
  end

  def bar
    hek = 5
    foo 1 
    baz
  end
    
end

if __FILE__ == $0 then
  obj = B.new
  
  obj.foo
  p obj.bar
end
EOS
  
  def test_classes_define_method
    scriptfile = RRB::ScriptFile.new( INPUT_STR, 'test.rb' )
    assert_equals( Set[RRB::NS['A'], RRB::NS['B']],
                   scriptfile.classes_define_method( 'foo' ) )
    assert_equals( Set[RRB::NS['A']],
                   scriptfile.classes_define_method( 'baz' ) )
    assert_equals( Set[],scriptfile.classes_define_method( 'id' ) )
  end
  
end

class TestScript_RenameMethodAll < RUNIT::TestCase
  RENAME_METHOD_ALL_INPUT = "\
/home/ohai/ruby/main.rb\C-a
require 'sub.rb'

class B < A

  def foo( x )
    @i += x*2
  end

  def bar
    hek = 5
    foo 1 
    baz
  end
    
end

if __FILE__ == $0 then
  obj = B.new
  
  obj.foo
  p obj.bar
end

\C-a/home/ohai/ruby/sub.rb\C-a

class A

  def initialize
    @i = 0
  end
  
  def foo( x )
    @i += x
  end

  def baz
    @i
  end
  
end
\C-a-- END --\C-a
"

  RENAME_METHOD_ALL_OUTPUT = "\
/home/ohai/ruby/main.rb\C-a
require 'sub.rb'

class B < A

  def foofee( x )
    @i += x*2
  end

  def bar
    hek = 5
    foofee 1 
    baz
  end
    
end

if __FILE__ == $0 then
  obj = B.new
  
  obj.foofee
  p obj.bar
end

\C-a/home/ohai/ruby/sub.rb\C-a

class A

  def initialize
    @i = 0
  end
  
  def foofee( x )
    @i += x
  end

  def baz
    @i
  end
  
end
\C-a-- END --\C-a
"

  def test_classes_define_method
    script = RRB::Script.new_from_io( StringIO.new( RENAME_METHOD_ALL_INPUT ) )
    assert_equals( Set[RRB::NS['A'], RRB::NS['B']],
                   script.classes_define_method( 'foo' ) )
    assert_equals( Set[RRB::NS['B']],
                   script.classes_define_method( 'bar' ) )
    assert_equals( Set[],
                   script.classes_define_method( 'id' ) )
  end
  
  def test_rename_method_all?
    script = RRB::Script.new_from_io( StringIO.new( RENAME_METHOD_ALL_INPUT ) )
    assert_equals( true, script.rename_method_all?( 'foo', 'foobar' ) )
    assert_equals( false, script.rename_method_all?( 'foo', 'bar' ) )
    assert_equals( false, script.rename_method_all?( 'foo', 'baz' ) )
    assert_equals( false, script.rename_method_all?( 'foo', 'Foo' ) )
    assert_equals( false, script.rename_method_all?( 'foo', 'hek' ) )
    assert_equals( false, script.rename_method_all?( 'send', 'foobar' ) )
  end

  def test_rename_method_all
    script = RRB::Script.new_from_io( StringIO.new( RENAME_METHOD_ALL_INPUT ) )
    script.rename_method_all( 'foo', 'foofee' )
    dst = ''      
    script.result_to_io( dst )
    assert_equals( RENAME_METHOD_ALL_OUTPUT, dst )
  end

end

if $0 == __FILE__
  if ARGV.size == 0
    suite = TestScript_RenameMethodAll.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestRenameMethodAll.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
