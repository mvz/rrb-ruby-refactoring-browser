require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/completion'

class TestScriptFile_Completion < RUNIT::TestCase

  INPUT = "
class A
  def method_1( arg1, arg2 )
    var = arg1 * arg2
  end
  def method_2
    @var = 1
  end
end

class B < A
  def method_1( arg1, arg3 )
    print arg1
    @var = uhe
    @var2 = heke
  end
end
class C < A
  module D
    def method_3
      p @var3 
    end
  end
end
"
  
  def test_refactable_methods
    scriptfile = RRB::ScriptFile.new( StringIO.new( INPUT ), "/tmp/test.rb" )
    methods = scriptfile.refactable_methods.map do |method|
      [ method.fullname, method.local_vars ]
    end
    assert_equals( [[ 'A#method_1', Set[ 'arg1', 'arg2', 'var' ] ],
		    [ 'A#method_2', Set[]],
		    [ 'B#method_1', Set[ 'arg1', 'arg3' ] ],
		    [ 'C::D#method_3', Set[] ]
		  ],
		  methods )
  end

  def test_refactable_classes_instance_vars
    scriptfile = RRB::ScriptFile.new( StringIO.new( INPUT ), "/tmp/test.rb" )
    ivars = scriptfile.refactable_classes_instance_vars
    assert_equals( { 'A' => Set[ '@var' ], 'B' =>  Set[ '@var', '@var2' ],
		  'C::D' => Set[ '@var3' ]},
		  ivars )
  end

  def test_refactable_consts
    scriptfile = RRB::ScriptFile.new( StringIO.new( INPUT ), "/tmp/test.rb" )
    dumped_info = RRB::Script.new( [scriptfile] ).get_dumped_info
    assert_equals( Set['::A','::B','::C','::C::D'],
                   scriptfile.refactable_consts(dumped_info) )
  end
  
end

class TestScript_Completion < RUNIT::TestCase

  INPUT = "\
/tmp/tmp/test1.rb\C-a
require 'test2'
Hoge = 0
class B < A
  def method_1( arg1, arg3 )
    print arg1
    @var = 3
  end
  @var2 = 4
end
\C-a/tmp/test2.rb\C-a
class A
  def method_1( arg1, arg2 )
    var = arg1 * arg2
    @varr = var * arg2
    @varrr = @varr ** 2
  end
  def method_2
  end
end
\C-a-- END --\C-a
"
  def test_refactable_methods
    script = RRB::Script.new_from_io( StringIO.new( INPUT ) )
    methods = script.refactable_methods.map do |method|
      [ method.fullname, method.local_vars ]
    end
    assert_equals( [ [ 'B#method_1', Set[ 'arg1', 'arg3' ] ],
		      [ 'A#method_1', Set[ 'arg1', 'arg2', 'var' ] ],
		      [ 'A#method_2', Set[] ] ],
		  methods )
  end

  def test_refactable_classes_instance_vars
    script = RRB::Script.new_from_io( StringIO.new( INPUT ) )
    assert_equals( { 'A' => Set[ '@varr', '@varrr' ], 'B' => Set[ '@var' ] },
		  script.refactable_classes_instance_vars )
    
  end

  def test_refactable_consts
    script = RRB::Script.new_from_io( StringIO.new( INPUT ) )
    assert_equals( Set['::A', '::B', '::Hoge'],
		  script.refactable_consts )
  end
  
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = RUNIT::TestSuite.new
    suite.add_test TestScriptFile_Completion.suite
    suite.add_test TestScript_Completion.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestScriptFile_Completion.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
