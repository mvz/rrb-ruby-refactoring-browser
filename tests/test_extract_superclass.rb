require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/extract_superclass'

class TestScriptFile_ExtractSuperclass < RUNIT::TestCase
  
end

class TestScript_ExtractSuperclass < RUNIT::TestCase
  INPUT_STR = <<EOS
/home/heke/temp/file1.rb#{RRB::IO_SPLITTER}
class A
  class D
    class E < A
      def heke
        p CONST
      end
    end
  end
end

class B < A
end

class C < A
end
CONST = 3
#{RRB::IO_SPLITTER}#{RRB::IO_TERMINATOR}#{RRB::IO_SPLITTER}
EOS

  def test_extract_superclass?
    script = RRB::Script.new_from_io( StringIO.new( INPUT_STR ))
    assert_equals( true,
                   script.extract_superclass?( RRB::NS[''], 'NEW',
                                               [RRB::NS['B'],
                                                RRB::NS['C'],
                                                RRB::NS['A::D::E']] ))
    assert_equals( false,
                   script.extract_superclass?( RRB::NS[''], 'NEW',
                                               [RRB::NS['K']]))
    assert_equals( false,
                   script.extract_superclass?( RRB::NS[''], 'NEW',
                                               [RRB::NS['B'],
                                                RRB::NS['C'],
                                                RRB::NS['A::D']] ))
    assert_equals( false,
                   script.extract_superclass?( RRB::NS[''], 'C',
                                               [RRB::NS['B'],
                                                RRB::NS['A::D']] ))
    assert_equals( false,
                   script.extract_superclass?( RRB::NS['A'], 'CONST',
                                               [RRB::NS['B'],RRB::NS['C']] ))

    assert_equals( false,
                   script.extract_superclass?( RRB::NS['A'], 'D',
                                               [RRB::NS['B']] ))
    assert_equals( false,
                   script.extract_superclass?( RRB::NS['A::D'], 'D',
                                               [RRB::NS['B']] )) 
  end
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = RUNIT::TestSuite.new
    suite.add_test( TestScriptFile_ExtractSuperclass.suite )
    suite.add_test( TestScript_ExtractSuperclass.suite )
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestScript_ExtractSuperclass.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
