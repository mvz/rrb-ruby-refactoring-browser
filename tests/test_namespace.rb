require 'runit/testcase'
require 'runit/cui/testrunner'


class TestNamespace < RUNIT::TestCase

  def test_initialize
    assert_exception( TypeError ){ RRB::Namespace.new( 3 ) }
  end
  
  def test_str
    assert_equals( "A::B::C", RRB::Namespace.new(['A','B','C']).str )
    assert_equals( "A::B::C", RRB::Namespace['A::B::C'].str )
  end
  
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = TestNamespace.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestNamespace.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
