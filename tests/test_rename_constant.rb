require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/rename_constant'

class TestScript_RenameConstant < RUNIT::TestCase
  INPUT_STR = <<EOD
/home/yhara/temp/file1.rb#{RRB::IO_SPLITTER}

Hoge = "toplevel"
class A
  Hoge = "A's"
  Hoge2 = Hoge + "const"
  class C
    def foo
      a = Hoge
    end
    class B < C
      Hoge = "B's"
      tmp = A::Hoge
    end
  end
end
class D < A::C::B
  def bar
    p Hoge
  end
end
class E
  Hoge2 = Hoge
end

if __FILE__ == $0 then
  A.new.foo
  p A::Hoge
  p A::C::Hoge
end
#{RRB::IO_SPLITTER}#{RRB::IO_TERMINATOR}#{RRB::IO_SPLITTER}
EOD

  OUTPUT_STR_Moge = <<EOD
/home/yhara/temp/file1.rb#{RRB::IO_SPLITTER}

Hoge = "toplevel"
class A
  Moge = "A's"
  Hoge2 = Moge + "const"
  class C
    def foo
      a = Moge
    end
    class B < C
      Hoge = "B's"
      tmp = A::Moge
    end
  end
end
class D < A::C::B
  def bar
    p Hoge
  end
end
class E
  Hoge2 = Hoge
end

if __FILE__ == $0 then
  A.new.foo
  p A::Moge
  p A::C::Hoge
end
#{RRB::IO_SPLITTER}#{RRB::IO_TERMINATOR}#{RRB::IO_SPLITTER}
EOD

  OUTPUT_STR_X = <<EOD
/home/yhara/temp/file1.rb#{RRB::IO_SPLITTER}

Hoge = "toplevel"
class A
  Hoge = "A's"
  Hoge2 = Hoge + "const"
  class X
    def foo
      a = Hoge
    end
    class B < X
      Hoge = "B's"
      tmp = A::Hoge
    end
  end
end
class D < A::X::B
  def bar
    p Hoge
  end
end
class E
  Hoge2 = Hoge
end

if __FILE__ == $0 then
  A.new.foo
  p A::Hoge
  p A::X::Hoge
end
#{RRB::IO_SPLITTER}#{RRB::IO_TERMINATOR}#{RRB::IO_SPLITTER}
EOD

  def test_rename_constant

    script = RRB::Script.new_from_io( StringIO.new( INPUT_STR ))
    script.rename_constant('A::Hoge', 'Moge')
    result = ''; script.result_to_io(result)
    assert_equals( OUTPUT_STR_Moge, result )

    script = RRB::Script.new_from_io( StringIO.new( INPUT_STR ))
    script.rename_constant('::A::C', 'X')
    result = ''; script.result_to_io(result)
    assert_equals( OUTPUT_STR_X, result )
  end

  def test_rename_constant?
    script = RRB::Script.new_from_io( StringIO.new( INPUT_STR ) )
    assert_equals(true, script.rename_constant?('A::Hoge', 'HogeHoge'))
    assert_equals(false, script.rename_constant?('A::Hoge', 'Hoge2'))
    assert_equals(false, script.rename_constant?('::E', 'D'))
  end
  
end

class TestConstResolver < RUNIT::TestCase
  include RRB::ConstResolver

  def test_class_of
    assert_equals('A::B::C', class_of('A::B::C::Hoge'))
    assert_equals('A::B::C', class_of('::A::B::C::Hoge'))
    assert_equals('Object', class_of('A'))
    assert_equals('Object', class_of('::A'))
  end
end

class TestScriptFile_RenameConstant < RUNIT::TestCase
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = RUNIT::TestSuite.new
    suite.add_test( TestScriptFile_RenameConstant.suite )
    suite.add_test( TestScript_RenameConstant.suite )
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestScriptFile_RenameConstant.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
