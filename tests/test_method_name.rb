require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/node.rb'

class TestMethodName < RUNIT::TestCase

  def test_name
    assert_equals( "A::B#me",
                   RRB::MethodName.new(RRB::Namespace.new("A::B"), 'me').name)
    assert_equals( "#method",
                   RRB::MethodName.new(RRB::Namespace::Toplevel, 'method').name)
  end

  def test_EQ
    assert( RRB::MN.new(RRB::NS.new("A::B"), 'me') == RRB::MN.new(RRB::NS.new("A::B"), 'me') )
    assert( RRB::MN.new(RRB::NS.new("A::B"), 'me') != RRB::MN.new(RRB::NS.new("A"), 'me') )
    assert( RRB::MN.new(RRB::NS.new("A::B"), 'me') != RRB::MN.new(RRB::NS.new("A::B"), 'e') )
    assert( RRB::MN.new(RRB::NS.new("A::B"), 'me') != RRB::CMN.new(RRB::NS.new("A::B"), 'me') ) 
  end

  def test_inspect
    assert_equals( '#<RRB::MethodName A::B#me>',
                   RRB::MN.new(RRB::NS.new("A::B"), 'me').inspect )
  end
  
  def test_s_AREF
    assert_equals( RRB::MN.new(RRB::NS.new("A::B"), 'me'),
                   RRB::MethodName['A::B#me'] )
    assert_equals( RRB::MN.new(RRB::NS::Toplevel, 'me'),
                   RRB::MethodName['#me'] )
    assert_equals( RRB::CMN.new(RRB::NS.new("A::B"), 'me'),
                   RRB::MethodName['A::B.me'] )
  end
end

class TestClassMethodName < RUNIT::TestCase
  def test_name
    assert_equals( "A::B.me",
                   RRB::ClassMethodName.new(RRB::Namespace.new("A::B"), 'me').name)
  end

  def test_EQ
    assert( RRB::CMN.new(RRB::NS.new("A::B"), 'me') == RRB::CMN.new(RRB::NS.new("A::B"), 'me') )
    assert( RRB::CMN.new(RRB::NS.new("A::B"), 'me') != RRB::CMN.new(RRB::NS.new("A"), 'me') )
    assert( RRB::CMN.new(RRB::NS.new("A::B"), 'me') != RRB::CMN.new(RRB::NS.new("A::B"), 'e') )
    assert( RRB::CMN.new(RRB::NS.new("A::B"), 'me') != RRB::MN.new(RRB::NS.new("A::B"), 'me') ) 
  end

  def test_inspect
    assert_equals( '#<RRB::ClassMethodName A::B.me>',
                   RRB::CMN.new(RRB::NS.new("A::B"), 'me').inspect )
  end
  
end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestMethodName.suite )
  suite.add_test( TestClassMethodName.suite )
  RUNIT::CUI::TestRunner.run(suite)
end
