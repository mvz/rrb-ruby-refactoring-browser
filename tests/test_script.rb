require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/script.rb'

class TestScript < RUNIT::TestCase

  RENAME_LOCAL_VAR_INPUT = "\
/home/ohai/ruby/test/file1.rb\C-l

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
\C-l/home/ohai/ruby/test/file2.rb\C-l
class Rename
  def method_2( x, y)
    x**2 + y**2
  end
end
\C-l-- END --\C-l
"

  RENAME_LOCAL_VAR_OUTPUT = "\
/home/ohai/ruby/test/file1.rb\C-l

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
\C-l/home/ohai/ruby/test/file2.rb\C-l
class Rename
  def method_2( yy, y)
    yy**2 + y**2
  end
end
\C-l-- END --\C-l
"

  def test_rename_local_var?
    script = RRB::Script.new_from_io( StringIO.new( RENAME_LOCAL_VAR_INPUT ) )
    assert_equals( true,
		  script.rename_local_var?(['Rename'],'method_2','x','yy') )
    assert_equals( false,
		  script.rename_local_var?(['Rename'],'method_1','x','y') )
  end

  
  def test_rename_local_var
    script = RRB::Script.new_from_io( StringIO.new( RENAME_LOCAL_VAR_INPUT ) )
    script.rename_local_var( ['Rename'],'method_2','x','yy' )
    dst = ''      
    script.result_to_io( dst )
    assert_equals( RENAME_LOCAL_VAR_OUTPUT, dst )
  end

  RENAME_METHOD_ALL_INPUT = "\
/home/ohai/ruby/main.rb\C-l
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

\C-l/home/ohai/ruby/sub.rb\C-l

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
\C-l-- END --\C-l
"

  def test_rename_method_all?
    script = RRB::Script.new_from_io( StringIO.new( RENAME_METHOD_ALL_INPUT ) )
    assert_equals( true, script.rename_method_all?( 'foo', 'foobar' ) )
    assert_equals( false, script.rename_method_all?( 'foo', 'bar' ) )
    assert_equals( false, script.rename_method_all?( 'foo', 'baz' ) )
    assert_equals( false, script.rename_method_all?( 'foo', 'Foo' ) )
    assert_equals( false, script.rename_method_all?( 'foo', 'hek' ) )
  end

  WORK_DIR = '/tmp/rrb_work'
  
  def test_dump
    script = RRB::Script.new_from_io( StringIO.new( RENAME_METHOD_ALL_INPUT ) )
    info = script.get_dumped_info( WORK_DIR )
    assert_equals( "class", info["B"].type )
    assert_equals( ["foo","bar"], info["B"].public_method_names )
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
