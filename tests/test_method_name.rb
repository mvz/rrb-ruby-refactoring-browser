require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/node.rb'

class TestMethod < RUNIT::TestCase

  def test_name
    assert_equals( "A::B#me",
                   RRB::Method.new(RRB::Namespace.new("A::B"), 'me').name)
    assert_equals( "#method",
                   RRB::Method.new(RRB::Namespace::Toplevel, 'method').name)
  end

  def test_EQ
    assert( RRB::MN.new(RRB::NS.new("A::B"), 'me') == RRB::MN.new(RRB::NS.new("A::B"), 'me') )
    assert( RRB::MN.new(RRB::NS.new("A::B"), 'me') != RRB::MN.new(RRB::NS.new("A"), 'me') )
    assert( RRB::MN.new(RRB::NS.new("A::B"), 'me') != RRB::MN.new(RRB::NS.new("A::B"), 'e') )
    assert( RRB::MN.new(RRB::NS.new("A::B"), 'me') != RRB::CMN.new(RRB::NS.new("A::B"), 'me') ) 
  end

  def test_hash
    assert(RRB::MN["A::B#me"].hash == RRB::MN.new(RRB::NS.new("A::B"),"me").hash)
  end

  def test_inspect
    assert_equals( '#<RRB::Method A::B#me>',
                   RRB::MN.new(RRB::NS.new("A::B"), 'me').inspect )
  end

  def test_ns_replaced
    assert_equals(RRB::Method['A::B#me'],
                  RRB::Method['C::D::E#me'].ns_replaced( RRB::NS["A::B"] ) )
  end

  def test_bare_name_replaced
    assert_equals(RRB::Method['A::B#mo'],
                  RRB::Method['A::B#me'].bare_name_replaced("mo" ))
  end
  
  def test_s_AREF
    assert_equals( RRB::MN.new(RRB::NS.new("A::B"), 'me'),
                   RRB::Method['A::B#me'] )
    assert_equals( RRB::MN.new(RRB::NS::Toplevel, 'me'),
                   RRB::Method['#me'] )
    assert_equals( RRB::CMN.new(RRB::NS.new("A::B"), 'me'),
                   RRB::Method['A::B.me'] )
  end

end

class TestClassMethod < RUNIT::TestCase
  def test_name
    assert_equals( "A::B.me",
                   RRB::ClassMethod.new(RRB::Namespace.new("A::B"), 'me').name)
  end

  def test_EQ
    assert( RRB::CMN.new(RRB::NS.new("A::B"), 'me') == RRB::CMN.new(RRB::NS.new("A::B"), 'me') )
    assert( RRB::CMN.new(RRB::NS.new("A::B"), 'me') != RRB::CMN.new(RRB::NS.new("A"), 'me') )
    assert( RRB::CMN.new(RRB::NS.new("A::B"), 'me') != RRB::CMN.new(RRB::NS.new("A::B"), 'e') )
    assert( RRB::CMN.new(RRB::NS.new("A::B"), 'me') != RRB::MN.new(RRB::NS.new("A::B"), 'me') ) 
  end

  def test_inspect
    assert_equals( '#<RRB::ClassMethod A::B.me>',
                   RRB::CMN.new(RRB::NS.new("A::B"), 'me').inspect )
  end

  def test_match_node?
    parser = RRB::Parser.new
    tree = parser.run File.open( "samples/visitor_sample.rb", "r" )

    test_class_a = RRB::Namespace.new("TestClassA")
    method_2 = tree.class_info("TestClassA").method_info("method_2")
    method_7 = tree.class_info("TestClassA").classmethod_info("method_7")

    assert(RRB::Method["TestClassA#method_2"].match_node?(test_class_a,method_2))
    assert(RRB::Method["TestClassA.method_7"].match_node?(test_class_a,method_7))
    assert(!RRB::Method["TestClassA.method_2"].match_node?(test_class_a,method_2))
    assert(!RRB::Method["TestClassA#method_7"].match_node?(test_class_a,method_7))
  end

  def test_ns_replaced
    assert_equals(RRB::Method['A::B.me'],
                  RRB::Method['C::D::E.me'].ns_replaced( RRB::NS["A::B"] ) )
  end

  def test_bare_name_replaced
    assert_equals(RRB::Method['A::B.mo'],
                  RRB::Method['A::B.me'].bare_name_replaced("mo" ))
  end
end

if $0 == __FILE__
  suite = RUNIT::TestSuite.new
  suite.add_test( TestMethodName.suite )
  suite.add_test( TestClassMethodName.suite )
  RUNIT::CUI::TestRunner.run(suite)
end
