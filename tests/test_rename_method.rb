require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/rename_method'

class TestScript_RenameMethod < RUNIT::TestCase

  INPUT_STR2 = "\
/home/ohai/ruby/test/test.rb\C-a
class E
end

class F < E
  def bar
    foo
  end
end

class G < F
  def foo
  end
end

if __FILE__ == $0 then
  G.new.foo
end
\C-a-- END --\C-a
"

  OUTPUT_STR2 = "\
/home/ohai/ruby/test/test.rb\C-a
class E
end

class F < E
  def bar
    foofee
  end
end

class G < F
  def foo(*arg); raise 'G#foo is renamed foofee' end
  def foofee
  end
end

if __FILE__ == $0 then
  G.new.foo
end
\C-a-- END --\C-a
"

  def input_str
    result = 'test.rb'
    result << RRB::IO_SPLITTER
    result << TestScriptFile_RenameMethod::INPUT_STR
    result << RRB::IO_SPLITTER
    result << RRB::IO_TERMINATOR
    result << RRB::IO_SPLITTER
    result
  end
  
  def test_classes_respond_to
    script = RRB::Script.new_from_io( StringIO.new( input_str ) )
    assert_equals( %w( C ).map{|x| RRB::NS.new(x)},
		  script.classes_respond_to( [ RRB::NS.new( 'C' ) ], 'foo' ) )
    assert_equals( %w( A B C::D ).map{|x| RRB::NS.new(x)},
                   script.classes_respond_to( [ RRB::NS.new( 'A' ) ], 'foo' ).sort )
    assert_equals( %w( A B C::D ).map{|x| RRB::NS.new(x)},
		  script.classes_respond_to( [ RRB::NS.new( 'B' ) ], 'foo' ).sort )
    assert_equals( %w( A B C::D ).map{|x| RRB::NS.new(x)},
		  script.classes_respond_to( [ RRB::NS.new( 'C::D' ) ], 'foo' ).sort )
    script2 = RRB::Script.new_from_io( StringIO.new( INPUT_STR2 ) )
    assert_equals( %w( F G ).map{|x| RRB::NS.new(x)},
		  script2.classes_respond_to( [ RRB::NS.new( 'G' ) ], 'foo' ).sort )
    assert_equals( %w( F G ).map{|x| RRB::NS.new(x)},
		  script2.classes_respond_to( [ RRB::NS.new( 'F' ) ], 'foo' ).sort )
  end

  def test_rename_method?
    script = RRB::Script.new_from_io( StringIO.new( input_str ) )
    assert_equals( true,
		  script.rename_method?( [ RRB::MN['C#foo'] ], 'bar' ) )
    assert_equals( true,
		  script.rename_method?( [ RRB::MN['B#foo'] ], 'foobar' ) )
    assert_equals( false,
		  script.rename_method?( [ RRB::MN['B#foo'] ], 'bar' ) )
    assert_equals( "bar: already defined at C::D\n", script.error_message)
    assert_equals( false,
		  script.rename_method?( [ RRB::MN['B#foo'] ], '@@bar' ) )
    assert_equals( "@@bar is not a valid name for methods\n",
                   script.error_message)
  end

  def test_rename_method
    script = RRB::Script.new_from_io( StringIO.new( INPUT_STR2 ) )
    script.rename_method( [ RRB::MN['G#foo'] ], 'foofee' )
    result = ''
    script.result_to_io( result )
    assert_equals( OUTPUT_STR2, result )
  end
  
end

class TestScriptFile_RenameMethod < RUNIT::TestCase

  INPUT_STR = "\
class A
  def foo    
  end
  def bar
  end
end

class B < A
  def foo
    super
  end
  def bar
    foo
  end
end

class C
  def foo
  end
  
  class D < B
    def bar
      foo
    end
  end
  
end


if __FILE__ == $0 then
  b = B.new
  a.foo
  c = C.new
  c.foo
end
"


  OUTPUT_STR = "\
class A
  def foo(*arg); raise 'A#foo is renamed feefoo' end
  def feefoo    
  end
  def bar
  end
end

class B < A
  def foo(*arg); raise 'B#foo is renamed feefoo' end
  def feefoo
    super
  end
  def bar
    feefoo
  end
end

class C
  def foo
  end
  
  class D < B
    def bar
      feefoo
    end
  end
  
end


if __FILE__ == $0 then
  b = B.new
  a.foo
  c = C.new
  c.foo
end
"

  def test_classes_call_method
    scriptfile = RRB::ScriptFile.new( INPUT_STR, 'test.rb' )
    assert_equals( [ RRB::NS['B'], RRB::NS['C::D'] ],
		  scriptfile.classes_call_method( 'foo' ) )
  end
  
  def test_rename_method
    scriptfile = RRB::ScriptFile.new( INPUT_STR, 'test.rb' )
    scriptfile.rename_method( %w(A#foo B#foo C::D#foo).map{|x| RRB::MN[x]}, 'feefoo' )
    assert_equals( OUTPUT_STR, scriptfile.new_script )
  end

end

if $0 == __FILE__
  if ARGV.size == 0
    suite = RUNIT::TestSuite.new
    suite.add_test( TestScriptFile_RenameMethod.suite )
    suite.add_test( TestScript_RenameMethod.suite )
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestScriptFile_RenameMethod.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
