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

  def test_EQ
    assert( RRB::Namespace.new("X::B::C") == RRB::Namespace.new(['X','B','C']) )
    assert( RRB::Namespace.new("X::B::C") != RRB::Namespace.new(['X','D','C']) )
  end

  def test_hash
    assert( RRB::Namespace.new("X::B::C").hash ==
	   RRB::Namespace.new(['X','B','C']).hash )
  end

  def  test_eql?
    assert( RRB::Namespace.new("X::B::C").eql?( RRB::Namespace.new(['X','B','C'])))
    assert( !RRB::Namespace.new("X::B::C").eql?(RRB::Namespace.new(['X','D','C'])))
  end

  def test_inspect
    assert_equals( '#<RRB::NS: X::Y::Z>', RRB::Namespace.new("X::Y::Z").inspect )
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
