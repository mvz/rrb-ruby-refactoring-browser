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

  OUTPUT_STR1 = <<EOS
/home/heke/temp/file1.rb#{RRB::IO_SPLITTER}
class A

  class D
    class E < ::B::NEW
      def heke
        p CONST
      end
    end
  end
end

class B < ::B::NEW
  
end

class C < ::B::NEW
end
CONST = 3
class B
  class NEW < ::A
  end
end

#{RRB::IO_SPLITTER}#{RRB::IO_TERMINATOR}#{RRB::IO_SPLITTER}
EOS


  OUTPUT_STR2 = <<EOS
/home/heke/temp/file1.rb#{RRB::IO_SPLITTER}
class A

  class NEW < ::A
  end
  class D
    class E < ::A::NEW
      def heke
        p CONST
      end
    end
  end
end

class B < ::A::NEW
  
end

class C < ::A::NEW
end
CONST = 3

#{RRB::IO_SPLITTER}#{RRB::IO_TERMINATOR}#{RRB::IO_SPLITTER}
EOS

  OUTPUT_STR3 = <<EOS
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

class P < ::A
end
class B < ::P
  
end

class C < ::P
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
                                                RRB::NS['A::D::E']],
                                               '/home/heke/temp/file1.rb',
                                               19 ))
    assert_equals( true,
                   script.extract_superclass?( RRB::NS['B'], 'NEW',
                                               [RRB::NS['B']],
                                               '/home/heke/temp/file1.rb',
                                               19 ))
    assert_equals( false,
                   script.extract_superclass?( RRB::NS[''], 'NEW',
                                               [RRB::NS['K']],
                                               '/home/heke/temp/file1.rb',
                                               19 ))
    assert_equals( "K: No such class\n", 
                   script.error_message)
    assert_equals( false,
                   script.extract_superclass?( RRB::NS[''], 'NEW',
                                               [RRB::NS['B'],
                                                RRB::NS['C'],
                                                RRB::NS['A::D']],
                                               '/home/heke/temp/file1.rb',
                                               19 ))
    assert_equals( "B and A::D are not sibling classes\n",
                   script.error_message)
    assert_equals( false,
                   script.extract_superclass?( RRB::NS[''], 'C',
                                               [RRB::NS['B'],
                                                RRB::NS['A::D']],
                                               '/home/heke/temp/file1.rb',
                                               19 ))
    assert_equals( "B and A::D are not sibling classes\n",
                   script.error_message)

    assert_equals( false,
                   script.extract_superclass?( RRB::NS['A'], 'CONST',
                                               [RRB::NS['B'],RRB::NS['C']],
                                               '/home/heke/temp/file1.rb',
                                               19 ))
    assert_equals( "CONST: already exists\n",
                   script.error_message)

    assert_equals( false,
                   script.extract_superclass?( RRB::NS['A'], 'D',
                                               [RRB::NS['B']],
                                               '/home/heke/temp/file1.rb',
                                               19 ))
    assert_equals( "D: already exists\n",
                   script.error_message)

    assert_equals( false,
                   script.extract_superclass?( RRB::NS['A::D'], 'D',
                                               [RRB::NS['B']],
                                               '/home/heke/temp/file1.rb',
                                               19 ))
    assert_equals( "D: already exists\n",
                   script.error_message)

    assert_equals( true,
                   script.extract_superclass?( RRB::NS['A'], 'NEW',
                                               [RRB::NS['B']],
                                               '/home/heke/temp/file1.rb',
                                               3 ))
    assert_equals( false,
                   script.extract_superclass?( RRB::NS['A'], 'NEW',
                                               [RRB::NS['B']],
                                                 '/home/heke/temp/file1.rb',
                                                 14 ))
    assert_equals( "Invalid Position to define new class\n",
                   script.error_message)

  end

  def test_extract_superclass
    script = RRB::Script.new_from_io( StringIO.new( INPUT_STR ))

    script.extract_superclass( RRB::NS['B'], 'NEW',
                               [RRB::NS['A::D::E'],RRB::NS['B'],RRB::NS['C']],
                               '/home/heke/temp/file1.rb', 19 )
    result = ''; script.result_to_io(result)
    assert_equals( OUTPUT_STR1, result )

    script = RRB::Script.new_from_io( StringIO.new( INPUT_STR ))
    script.extract_superclass( RRB::NS['A'], 'NEW',
                               [RRB::NS['A::D::E'],RRB::NS['B'],RRB::NS['C']],
                               '/home/heke/temp/file1.rb', 3 )
    result = ''; script.result_to_io(result)
    assert_equals( OUTPUT_STR2, result )

    script = RRB::Script.new_from_io( StringIO.new( INPUT_STR ))
    script.extract_superclass( RRB::NS[''], 'P',
                               [RRB::NS['B'],RRB::NS['C']],
                               '/home/heke/temp/file1.rb', 12 )
    result = ''; script.result_to_io(result)
    assert_equals( OUTPUT_STR3, result )
  end

  NEW_SUPERCLASS_DEF = [
    "class D\n",
    "  class NEW < ::C\n",
    "  end\n",
    "end\n",
  ]
  def test_new_superclass_def
    script = RRB::Script.new_from_io( StringIO.new( INPUT_STR ))
    assert_equals( NEW_SUPERCLASS_DEF,
                   script.superclass_def( RRB::NS["A::D"],
                                          "NEW",
                                          RRB::NS["C"],
                                          RRB::NS["A"] ))
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
