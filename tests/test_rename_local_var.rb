require 'runit/testcase'
require 'runit/cui/testrunner'

class TestScriptFile_RenameLocalVariable < RUNIT::TestCase
  
end

class TestScript_RenameLocalVariable < RUNIT::TestCase
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
  def test_rename_local_var
    script = RRB::Script.new_from_io( StringIO.new( RENAME_LOCAL_VAR_INPUT ) )
    script.rename_local_var( RRB::NS.new('Rename'),
                             RRB::MN.new('method_2', true),
                             'x','yy' )
    dst = ''      
    script.result_to_io( dst )
    assert_equals( RENAME_LOCAL_VAR_OUTPUT, dst )

    script = RRB::Script.new_from_filenames("samples/rename_var_sample.rb")   
    script.rename_local_var( RRB::NS.new('Rename'),
                             RRB::MN.new('method_1', true),
                             'z', 'bb') 
    dst = ''
    script.result_to_io(dst)
    assert_equals( File.open( 'samples/rename_var_sample_after.rb' ).read,dst)
  end

  def test_rename_local_var?
    script = RRB::Script.new_from_io( StringIO.new( RENAME_LOCAL_VAR_INPUT ) )
    assert_equals( true,
		  script.rename_local_var?(RRB::NS.new('Rename'),
                                           RRB::MN.new('method_2', true),
					   'x','yy') )

    assert_equals( false,
		  script.rename_local_var?(RRB::NS.new('Rename'),
                                           RRB::MN.new('method_1', true),
					   'x','y') )
    assert_equals("y: already used\n", script.error_message)    
    script = RRB::Script.new_from_filenames("samples/rename_var_sample.rb")   
    
    assert_equals( true, script.rename_local_var?( RRB::NS.new('Rename'),
                                                   RRB::MN.new('method_1', true),
                                                   'z', 'bb' ) )
    # collision with other variable
    assert_equals( false, script.rename_local_var?( RRB::NS.new('Rename'),
                                                    RRB::MN.new('method_1', true),
                                                    'z', 'x' ) )
    assert_equals("x: already used\n", script.error_message)    
    assert_equals( false, script.rename_local_var?( RRB::NS.new('Rename'),
                                                    RRB::MN.new('method_1', true),
                                                    'z', 'i' ) )
    assert_equals("i: already used\n", script.error_message)    
    # invalid identifier 
    assert_equals( false, script.rename_local_var?( RRB::NS.new('Rename'),
                                                    RRB::MN.new('method_1', true),
                                                    'z', 'Z' ) )
    assert_equals("Z: not a valid name for local variables\n",
                  script.error_message)    
    assert_equals( false, script.rename_local_var?( RRB::NS.new('Rename'),
                                                    RRB::MN.new('method_1', true),
                                                    'z', 'print' ) )
    
    assert_equals("print: already used as a function\n",
                  script.error_message)
    assert_equals( false, script.rename_local_var?( RRB::NS.new('Rename'),
                                                    RRB::MN.new('method_1', true),
                                                    'z', 'super' ) )
    assert_equals("super: not a valid name for local variables\n",
                  script.error_message)

    
  end

end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestScriptFile_RenameLocalVariable )
  suite.add_test( TestScript_RenameLocalVariable )
  RUNIT::CUI::TestRunner.run(suite)
end

