require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/scriptfile'
require 'stringio'

class TestFunction < RUNIT::TestCase

BEFORE_REPLACE = <<'EOS'

# comment
class Rename
  def method_1( x, y )
    zz = 3
    zz.upto(6) do |zzz|
      print zzz*3, "\n"
    end
    print zz**4, zz**5
  end
end
EOS

AFTER_REPLACE = <<'EOS'

# comment
class Rename
  def method_1( x, y )
    c = 3
    c.upto(6) do |zzz|
      print zzz*3, "\n"
    end
    print c**4, c**5
  end
end
EOS


  REPLACE_INFO = [
    RRB::Replacer.new( 5, 6, 'zz', 'c' ),
    RRB::Replacer.new( 6, 6, 'zz', 'c' ),
    RRB::Replacer.new( 9, 12, 'zz', 'c' ),
    RRB::Replacer.new( 9, 19, 'zz', 'c' ),
  ]

  def test_replace_str

    assert_equals( AFTER_REPLACE,
		  RRB.replace_str( BEFORE_REPLACE, REPLACE_INFO) )
  end
  
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = TestFunction.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestFunction.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
