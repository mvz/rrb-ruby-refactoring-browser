require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/script.rb'
require 'rrb/rename_local_var'
require 'rrb/rename_instance_var'
require 'rrb/rename_class_var'
require 'rrb/rename_method_all'
require 'rrb/pullup_method'
require 'rrb/pushdown_method'

class TestScript < RUNIT::TestCase

  RENAME_LOCAL_VAR_INPUT = "\
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

  RENAME_LOCAL_VAR_OUTPUT = "\
/home/ohai/ruby/test/file2.rb\C-a
class Rename
  def method_2( yy, y)
    yy**2 + y**2
  end
end
\C-a-- END --\C-a
"

  def test_rename_local_var?
    script = RRB::Script.new_from_io( StringIO.new( RENAME_LOCAL_VAR_INPUT ) )
    assert_equals( true,
		  script.rename_local_var?(RRB::NS.new('Rename'),'method_2',
					   'x','yy') )
    assert_equals( false,
		  script.rename_local_var?(RRB::NS.new('Rename'),'method_1',
					   'x','y') )
  end

  
  def test_rename_local_var
    script = RRB::Script.new_from_io( StringIO.new( RENAME_LOCAL_VAR_INPUT ) )
    script.rename_local_var( RRB::NS.new('Rename'),'method_2','x','yy' )
    dst = ''      
    script.result_to_io( dst )
    assert_equals( RENAME_LOCAL_VAR_OUTPUT, dst )
  end

  def test_rename_instance_var
    script = RRB::Script.new_from_filenames('samples/rename_instance_var_sample.rb','samples/rename_instance_var_sample2.rb')
    script.rename_instance_var(RRB::NS.new('X::A'), '@a', '@f')
    dst = ''
    script.result_to_io(dst)
    assert_equals(File.open('samples/rename_instance_var_sample_after.rb').read, dst)
    script = RRB::Script.new_from_filenames('samples/rename_instance_var_sample.rb','samples/rename_instance_var_sample2.rb')
    script.rename_instance_var(RRB::NS.new(['X', 'B']), '@a', '@f')
    dst = ''
    script.result_to_io(dst)
    assert_equals(File.open('samples/rename_instance_var_sample_after.rb').read, dst)
                    
  end


  def test_rename_instance_var?
    script = RRB::Script.new_from_filenames('samples/rename_instance_var_sample.rb','samples/rename_instance_var_sample2.rb')
    assert_equals(true, script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@f'))
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@b'))
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@c'))
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@d'))
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@e'))
  end

  def test_rename_class_var
    script = RRB::Script.new_from_filenames('samples/rename_class_var_sample.rb')
    script.rename_class_var(RRB::NS['X::A'], '@@a', '@@f')
    dst = ''
    script.result_to_io(dst)
    assert_equals(File.open('samples/rename_class_var_sample_after.rb').read, dst)
    script = RRB::Script.new_from_filenames('samples/rename_class_var_sample.rb')
    script.rename_class_var(RRB::NS['X::B'], '@@a', '@@f')
    dst = ''
    script.result_to_io(dst)
    assert_equals(File.open('samples/rename_class_var_sample_after.rb').read, dst)
                    
  end


  def test_rename_class_var?
    script = RRB::Script.new_from_filenames('samples/rename_class_var_sample.rb')
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@b'))
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@c'))
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@d'))
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@e'))
    assert_equals(true, script.rename_class_var?(RRB::NS['X::A'], '@@a', '@@f'))
  end

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
  
  def test_pullup_method?
    script = RRB::Script.new_from_filenames("samples/pullup_method_sample.rb")
    assert_equals(true, script.pullup_method?(RRB::NS['Derived'], 'bar', RRB::NS['Base']))
    assert_equals(false, script.pullup_method?(RRB::NS['Derived'], 'foo', RRB::NS['Base']))
    assert_equals(false, script.pullup_method?(RRB::NS['Derived'], 'hoge', RRB::NS['Base']))
    assert_equals(false, script.pullup_method?(RRB::NS['Base'], 'hoge', RRB::NS['Derived']))
  end

  def test_pullup_method
    script = RRB::Script.new_from_filenames("samples/pullup_method_sample.rb")
    script.pullup_method(RRB::NS['Derived'], 'bar', RRB::NS['Base'])
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pullup_method_sample_after.rb' ).read,
		    dst )

  end

  def test_pushdown_method?
    script = RRB::Script.new_from_filenames("samples/pushdown_method_sample.rb")
    assert_equals(true, script.pushdown_method?(RRB::NS['B'], 'x', RRB::NS['C']))
    assert_equals(false, script.pushdown_method?(RRB::NS['B'], 'y', RRB::NS['C']))
    assert_equals(false, script.pushdown_method?(RRB::NS['B'], 'z', RRB::NS['C']))
    assert_equals(false, script.pushdown_method?(RRB::NS['B'], 'w', RRB::NS['C']))
    assert_equals(false, script.pushdown_method?(RRB::NS['C'], 'w', RRB::NS['B']))    
    assert_equals(true, script.pushdown_method?(RRB::NS['A'], 'a', RRB::NS['B']))
  end

  def test_pushdown_method
    script = RRB::Script.new_from_filenames("samples/pushdown_method_sample.rb")
    script.pullup_method(RRB::NS['B'], 'x', RRB::NS['C'])
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pushdown_method_sample_after.rb' ).read,
		    dst )    
  end


  def test_dump
    script = RRB::Script.new_from_io( StringIO.new( RENAME_METHOD_ALL_INPUT ) )
    info = script.get_dumped_info
    assert_equals( "class", info["B"].type )
    assert_equals( ["foo","bar"].sort, info["B"].public_method_names.sort )
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
