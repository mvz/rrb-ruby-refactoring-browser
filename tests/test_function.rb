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

  def test_expand_tabs
    assert_equals( " "*8 + "heke" , RRB.expand_tabs("\theke") )
    assert_equals( " "*12 + "heke\n", RRB.expand_tabs("\t    heke\n") )
    assert_equals( " "*16, RRB.expand_tabs( "\t\t"))
    assert_equals( " "*16, RRB.expand_tabs( "\t \t"))
    assert_equals( "heke\t\n", RRB.expand_tabs( "heke\t\n" ) )
  end

BEFORE_REINDENT1 = <<EOS.split(/^/)
\t      def hek
\t\treturn true
\t      end
EOS
  
BEFORE_REINDENT2 = <<EOS.split(/^/)
def hek
  return true
end
EOS

AFTER_REINDENT16 = <<EOS.split(/^/)
                def hek
                  return true
                end
EOS

AFTER_REINDENT12 = <<EOS.split(/^/)
            def hek
              return true
            end
EOS

AFTER_REINDENT2 = <<EOS.split(/^/)
  def hek
    return true
  end
EOS

  def test_reindent_lines
    assert_equals( AFTER_REINDENT16,
                   RRB.reindent_lines( BEFORE_REINDENT1, 16 ) )
    assert_equals( AFTER_REINDENT12,
                   RRB.reindent_lines( BEFORE_REINDENT1, 12 ) )
    assert_equals( AFTER_REINDENT2,
                   RRB.reindent_lines( BEFORE_REINDENT2, 2 ) )
  end

  def test_count_indent_str
    assert_equals( 0, RRB.count_indent_str( "def heke\n" ) )
    assert_equals( 14, RRB.count_indent_str( "\t      def hek\n" ) )
  end

  def test_count_indent
    assert_equals( 0, RRB.count_indent( BEFORE_REINDENT2 ) )
    assert_equals( 14, RRB.count_indent( BEFORE_REINDENT1 ) )
    assert_equals( 0, RRB.count_indent( ["\t\n", "  \t \t\n", "  \t\t\t"] ) )
    assert_equals( 3, RRB.count_indent( ["\n", "\t \t\n", "   heke\n"] ) )
  end

  def test_delete_indent
    assert_equals( BEFORE_REINDENT2,
                   RRB.delete_indent( BEFORE_REINDENT1 ) )
    assert_equals( BEFORE_REINDENT2,
                   RRB.delete_indent( BEFORE_REINDENT2 ) )
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
