require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/script.rb'
require 'rrb/rename_local_var'
require 'rrb/rename_instance_var'
require 'rrb/rename_class_var'
require 'rrb/rename_method_all'
require 'rrb/pullup_method'
require 'rrb/pushdown_method'
require 'rrb/rename_global_var'

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
    assert_equals("X::A already has @b\n", script.error_message)
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@c'))
    assert_equals("X::B already has @c\n", script.error_message)
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', '@d'))
    assert_equals("X::C already has @d\n", script.error_message)
    assert_equals(false,
		  script.rename_instance_var?(RRB::NS.new('X::B'), '@a', 'ee'))
    assert_equals("ee is not a valid name for instance variables\n",
                  script.error_message)

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
    assert_equals("X::A already has @@b\n", script.error_message)
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@c'))
    assert_equals("X::B already has @@c\n", script.error_message)
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@d'))
    assert_equals("X::C already has @@d\n", script.error_message)
    assert_equals(false, script.rename_class_var?(RRB::NS['X::B'], '@@a', '@@e'))
    assert_equals("X::D already has @@e\n", script.error_message)
    assert_equals(false, script.rename_class_var?(RRB::NS['X::A'], '@@a', 'fff'))
    assert_equals("fff is not a valid name for class variables\n", script.error_message)    
  end

  
  def test_pullup_method?
    script = RRB::Script.new_from_filenames("samples/pullup_method_sample.rb")
    assert_equals(true, script.pullup_method?(RRB::NS['Derived'], 'bar', RRB::NS['Base']))
    assert_equals(false, script.pullup_method?(RRB::NS['Derived'], 'foo', RRB::NS['Base']))
    assert_equals("Derived#foo uses bar defined at Derived\n", script.error_message)    
    assert_equals(false, script.pullup_method?(RRB::NS['Derived'], 'hoge', RRB::NS['Base']))
    assert_equals("Derived doesn't have a function called hoge\n", script.error_message)    
    assert_equals(false, script.pullup_method?(RRB::NS['Base'], 'hoge', RRB::NS['Derived']))
    assert_equals("Derived is not the superclass of Base\n", script.error_message)    
    assert_equals(false, script.pullup_method?(RRB::NS['Derived'], 'asdf', RRB::NS['Base']))
    assert_equals("Base already has asdf\n", script.error_message)    

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
    assert_equals("B#y calls private function \"z\"\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['B'], 'z', RRB::NS['C']))
    assert_equals("Other function uses B#z\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['B'], 'w', RRB::NS['C']))
    assert_equals("C already has w\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['C'], 'w', RRB::NS['B']))    
    assert_equals("B is not the subclass of C\n", script.error_message)
    assert_equals(false, script.pushdown_method?(RRB::NS['C'], 'asdf', RRB::NS['B']))    
    assert_equals("C doesn't have a function called asdf\n", script.error_message)

    assert_equals(true, script.pushdown_method?(RRB::NS['A'], 'a', RRB::NS['B']))
  end

  def test_pushdown_method
    script = RRB::Script.new_from_filenames("samples/pushdown_method_sample.rb")
    script.pushdown_method(RRB::NS['B'], 'x', RRB::NS['C'])
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/pushdown_method_sample_after.rb' ).read,
		    dst )    
  end

  def test_rename_global_var
    script = RRB::Script.new_from_filenames("samples/rename_global_var_sample.rb")   
    script.rename_global_var('$x', '$y' )
    dst = ''
    script.result_to_io(dst)
      assert_equals( File.open( 'samples/rename_global_var_sample_after.rb' ).read, dst)
  end

  def test_rename_global_var?
    script = RRB::Script.new_from_filenames("samples/rename_global_var_sample.rb")   
    assert_equals( true, script.rename_global_var?('$x', '$y'))
    assert_equals( false, script.rename_global_var?('$x', '$z'))
    assert_equals("$z is already used\n", 
                  script.error_message)
    assert_equals( false, script.rename_global_var?('$x', 'x'))
    assert_equals("x is not a valid name for global variables\n", 
                  script.error_message)
    assert_equals( false, script.rename_global_var?('$x', '@x'))
    assert_equals("@x is not a valid name for global variables\n", 
                  script.error_message)
    assert_equals( false, script.rename_global_var?('$x', '@@x'))
    assert_equals("@@x is not a valid name for global variables\n", 
                  script.error_message)
    assert_equals( false, script.rename_global_var?('$x', 'print'))
    assert_equals("print is not a valid name for global variables\n", 
                  script.error_message)
  end

  def test_rename_local_var
    script = RRB::Script.new_from_io( StringIO.new( RENAME_LOCAL_VAR_INPUT ) )
    script.rename_local_var( RRB::NS.new('Rename'),'method_2','x','yy' )
    dst = ''      
    script.result_to_io( dst )
    assert_equals( RENAME_LOCAL_VAR_OUTPUT, dst )

    script = RRB::Script.new_from_filenames("samples/rename_var_sample.rb")   
      script.rename_local_var( RRB::NS.new('Rename'),
				   'method_1', 'z', 'bb' )
      dst = ''
      script.result_to_io(dst)
      assert_equals( File.open( 'samples/rename_var_sample_after.rb' ).read,dst)
	
  end

  def test_rename_local_var?
    script = RRB::Script.new_from_io( StringIO.new( RENAME_LOCAL_VAR_INPUT ) )
    assert_equals( true,
		  script.rename_local_var?(RRB::NS.new('Rename'),'method_2',
					   'x','yy') )

    assert_equals( false,
		  script.rename_local_var?(RRB::NS.new('Rename'),'method_1',
					   'x','y') )
    assert_equals("y is already used\n", script.error_message)    
    script = RRB::Script.new_from_filenames("samples/rename_var_sample.rb")   
    
    assert_equals( true, script.rename_local_var?( RRB::NS.new('Rename'),
							 'method_1',
							 'z', 'bb' ) )
    # collision with other variable
    assert_equals( false, script.rename_local_var?( RRB::NS.new('Rename'),
							 'method_1',
							 'z', 'x' ) )
    assert_equals("x is already used\n", script.error_message)    
    assert_equals( false, script.rename_local_var?( RRB::NS.new('Rename'),
							 'method_1',
							  'z', 'i' ) )
    assert_equals("i is already used\n", script.error_message)    
    # invalid identifier 
    assert_equals( false, script.rename_local_var?( RRB::NS.new('Rename'),
							 'method_1',
							  'z', 'Z' ) )
    assert_equals("Z is not a valid name for local variables\n",
                  script.error_message)    
    assert_equals( false, script.rename_local_var?( RRB::NS.new('Rename'),
							 'method_1',
							  'z', 'print' ) )

    assert_equals("print is already used as a function\n",
                  script.error_message)
    assert_equals( false, script.rename_local_var?( RRB::NS.new('Rename'),
							 'method_1',
							  'z', 'super' ) )
    assert_equals("super is not a valid name for local variables\n",
                  script.error_message)

    
  end
  def test_rename_method_all

    script = RRB::Script.new_from_filenames("samples/rename_method_sample.rb")   
    script.rename_method_all( 'foo', 'feefoo' )
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/rename_method_sample_after.rb' ).read,dst)

    
  end

  def test_rename_method_all?
    script = RRB::Script.new_from_filenames("samples/rename_method_sample.rb")   
    assert_equals( true, script.rename_method_all?( 'foo', 'feefoo' ) )
    assert_equals( true, script.rename_method_all?( 'foo', 'foo?' ) )
    # collision with local variable
    assert_equals( false, script.rename_method_all?( 'baz', 'hek' ) )
    assert_equals("hek is already used as a local variable at B#bar\n",
                  script.error_message)

    assert_equals( false, script.rename_method_all?( 'baz', 'bar' ) )
    assert_equals("B already has bar\n", script.error_message)
    assert_equals( false, script.rename_method_all?( 'print', 'feefoo' ) )
    assert_equals("Don't rename kernel method\n",
                  script.error_message)
    # invalid method name 
    assert_equals( false, script.rename_method_all?( 'foo', 'Foo' ) )
    assert_equals("Foo is not a valid name for methods\n",
                  script.error_message)
    assert_equals( false, script.rename_method_all?( 'foo', 'foo=' ) )
    assert_equals("foo= is not a valid name for methods\n",
                  script.error_message)
    assert_equals( false, script.rename_method_all?( 'foo', 'when' ) )
    assert_equals("when is not a valid name for methods\n",
                  script.error_message)
    assert_equals( false, script.rename_method_all?( 'foo', 'foo?fee' ))
    assert_equals("foo?fee is not a valid name for methods\n",
                  script.error_message)
  end

TEST_CLASS_A_METHOD_1 = 
'  def method_1( a, b )
    c = 3; @x = 2
    call( 3, 2 )
    a.each{|x| print x}
    yield b+c
  end
'

  def test_get_string_of_method
    script = RRB::Script.new_from_filenames('samples/parser_sample.rb')
    assert_equals(TEST_CLASS_A_METHOD_1, script.get_string_of_method(RRB::NS.new("TestClassA"), 'method_1'))
  end

  def test_get_class_on_region
    script = RRB::Script.new_from_filenames('samples/parser_sample.rb')
    assert_equals('TestClassA', script.get_class_on_region('samples/parser_sample.rb', 3..6).name)
    assert_equals(true, script.get_class_on_region('samples/parser_sample.rb', 26..28).nil?)
    assert_equals('', script.get_class_on_region('samples/parser_sample.rb', 1..1).name)
  end

  def test_get_method_on_region
    script = RRB::Script.new_from_filenames('samples/parser_sample.rb')
    assert_equals('TestClassA#method_1', script.get_method_on_region('samples/parser_sample.rb', 8..11).name)
    assert_equals(true, script.get_method_on_region('samples/parser_sample.rb', 7..11).nil?)
    assert_equals("", script.get_method_on_region('samples/parser_sample.rb', 38..38).name)
  end


  def test_dump
    script = RRB::Script.new_from_io( StringIO.new( RENAME_LOCAL_VAR_INPUT ) )
    info = script.get_dumped_info
    assert_equals( "class", info["Rename"].type )
    assert_equals( ["method_1","method_2"].sort,
                   info["Rename"].public_method_names.sort )
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
