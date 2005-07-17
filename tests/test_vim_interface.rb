require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/vim_interface'

class TestVimInterface < RUNIT::TestCase
  def test_search_id
    assert_equals("v12dd",
                  RRB::VimInterface.search_id("v12dd kdd",1))
    assert_equals("v12dd",
                  RRB::VimInterface.search_id("v12dd kdd",0))
    assert_equals("v12dd",
                  RRB::VimInterface.search_id("v12dd kdd",0))
    assert_equals("v12dd",
                  RRB::VimInterface.search_id("v12dd kdd",4))
    assert_equals("",
                  RRB::VimInterface.search_id("v12dd kdd",5))
    assert_equals("kdd",
                  RRB::VimInterface.search_id("v12dd kdd",6))
    assert_equals("@p",
                  RRB::VimInterface.search_id(" @p @@q $r ",1))
    assert_equals("@p",
                  RRB::VimInterface.search_id(" @p @@q $r ",2))
    assert_equals("",
                  RRB::VimInterface.search_id(" @p @@q $r ",3))
    assert_equals("@@q",
                  RRB::VimInterface.search_id(" @p @@q $r ",4))
    assert_equals("@@q",
                  RRB::VimInterface.search_id(" @p @@q $r ",5))
    assert_equals("@@q",
                  RRB::VimInterface.search_id(" @p @@q $r ",6))
    assert_equals("",
                  RRB::VimInterface.search_id(" @p @@q $r ",7))
    assert_equals("$r",
                  RRB::VimInterface.search_id(" @p @@q $r ",8))
    assert_equals("$r",
                  RRB::VimInterface.search_id(" @p @@q $r ",9))
    assert_equals("",
                  RRB::VimInterface.search_id(" @p @@q $r ",10))
    assert_equals("Heke",
                  RRB::VimInterface.search_id(" Heke  ",1))
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
