require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/rename_method'

class TestScript_RenameMethod < RUNIT::TestCase
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
  def feefoo    
  end
  def bar
  end
end

class B < A
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
  def test_rename_method

    script_file = RRB::ScriptFile.new( StringIO.new( INPUT_STR ),
				      '/home/ohai/ruby/rename_method.rb' )
    
    applied_classes = [
      RRB::ClassName.new('A'),
      RRB::ClassName.new('B'),
      RRB::ClassName.new('C::D'),
    ]
    script_file.rename_method( applied_classes, 'foo', 'feefoo' )
    assert_equals( OUTPUT_STR, script_file.new_script )
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
