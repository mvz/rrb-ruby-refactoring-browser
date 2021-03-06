require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/completion'

class TestScriptFile_Completion < RUNIT::TestCase

  INPUT = "
$a = 0
class A
  CONST = 0
  def method_1( arg1, arg2 )
    var = arg1 * arg2 + $a
  end
  def method_2
    @var = 1
    $b = 1
  end
  def A.method_3(arg1, arg2)
  end
end

class B < A
  def method_1( arg1, arg3 )
    print arg1
    @var = uhe
    @var2 = heke
  end
  def B.method_3(arg1, arg3)
  end
end
class C < A
  module D
    def method_4
      p @var3 
    end
  end
end
"
  
  def test_refactable_instance_methods
    scriptfile = RRB::ScriptFile.new( INPUT, "/tmp/test.rb" )
    methods = scriptfile.refactable_instance_methods.map do |method|
      [ method.name, method.local_vars ]
    end
    assert_equals( [[ 'A#method_1', Set[ 'arg1', 'arg2', 'var' ] ],
		    [ 'A#method_2', Set[]],
		    [ 'B#method_1', Set[ 'arg1', 'arg3' ] ],
		    [ 'C::D#method_4', Set[] ]
		  ],
		  methods )
  end

  def test_refactable_class_methods
    scriptfile = RRB::ScriptFile.new( INPUT, "/tmp/test.rb" )
    methods = scriptfile.refactable_class_methods.map do |method|
      [ method.name, method.local_vars ]
    end
    assert_equals( [[ 'A.method_3', Set[ 'arg1', 'arg2' ] ],
		    [ 'B.method_3', Set[ 'arg1', 'arg3' ] ],
		  ],
		  methods )
  end

  def test_refactable_classes
    scriptfile = RRB::ScriptFile.new( INPUT, "/tmp/test.rb" )
    classes = scriptfile.refactable_classes
    assert_equals( Set['C::D', 'A','B','C',],
		  classes )
  end

  def test_refactable_classes_instance_vars
    scriptfile = RRB::ScriptFile.new( INPUT, "/tmp/test.rb" )
    ivars = scriptfile.refactable_classes_instance_vars
    assert_equals( { 'A' => Set[ '@var' ], 'B' =>  Set[ '@var', '@var2' ],
		  'C::D' => Set[ '@var3' ]},
		  ivars )
  end

  def test_refactable_global_vars
    scriptfile = RRB::ScriptFile.new( INPUT, "/tmp/test.rb" )
    assert_equals( Set['$a', '$b'],
                   scriptfile.refactable_global_vars )
  end
  
  def test_refactable_consts
    scriptfile = RRB::ScriptFile.new( INPUT, "/tmp/test.rb" )
    dumped_info = RRB::Script.new( [scriptfile] ).get_dumped_info
    assert_equals( Set['C::D','A','B', 'A::CONST', 'C'],
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
    print arg1, $x, $y
    @var = 3
  end
  def B.method_3(arg1)
  end
  @var2 = 4
end
\C-a/tmp/test2.rb\C-a
$x = 1
class A
  def method_1( arg1, arg2 )
    var = arg1 * arg2
    @varr = var * arg2
    @varrr = @varr ** 2
  end
  def method_2
    $y = 2
  end
  def A.method_3(arg1)
  end
end
\C-a-- END --\C-a
"
  def test_refactable_instance_methods
    script = RRB::Script.new_from_io( StringIO.new( INPUT ) )
    methods = script.refactable_instance_methods.map do |method|
      [ method.name, method.local_vars ]
    end
    assert_equals( [ [ 'B#method_1', Set[ 'arg1', 'arg3' ] ],
		      [ 'A#method_1', Set[ 'arg1', 'arg2', 'var' ] ],
		      [ 'A#method_2', Set[] ] ],
		  methods )
  end

  def test_refactable_class_methods
    script = RRB::Script.new_from_io( StringIO.new( INPUT ) )
    methods = script.refactable_class_methods.map do |method|
      [ method.name, method.local_vars ]
    end
    assert_equals( [ [ 'B.method_3', Set[ 'arg1' ] ],
                    [ 'A.method_3', Set[ 'arg1' ] ] ],
		  methods )
  end


  def test_refactable_classes
    script = RRB::Script.new_from_io( StringIO.new( INPUT ) )
    assert_equals( Set['A', 'B'],
		  script.refactable_classes )
  end
  
  def test_refactable_classes_instance_vars
    script = RRB::Script.new_from_io( StringIO.new( INPUT ) )
    assert_equals( { 'A' => Set[ '@varr', '@varrr' ], 'B' => Set[ '@var' ] },
		  script.refactable_classes_instance_vars )
  end

  def test_refactable_global_vars
    script = RRB::Script.new_from_io( StringIO.new( INPUT ) )
    assert_equals( Set['$x', '$y'],
		  script.refactable_global_vars )
  end
  
  def test_refactable_consts
    script = RRB::Script.new_from_io( StringIO.new( INPUT ) )
    assert_equals( Set['A', 'B', 'Hoge'],
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
