require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/dumped_info.rb'
require 'rrb/dump_modules'

WORK_DIR = '/tmp/rrbtest'
RUBY_SCRIPT_NAME = "#{WORK_DIR}/script.rb"
DUMPED_FILE_NAME = "#{WORK_DIR}/dumped"
DUMPED_SCRIPT = <<EOS
class TestClassA
  def pubA
  end
  C1 = 2
  C2 = 4
end

class TestClassB < TestClassA
  def TestClassB.sing
  end
  def pub
  end
  protected
  def pro1
  end
  def pro2
  end
  private
  C3 = 1
  C1 = 3
end
class TestClassC < TestClassA
  include Enumerable
end
class TestClassD
  C1 = 7
  class TestClassE < TestClassA
  end
end
EOS

class TestDumpedInfo < RUNIT::TestCase

  # this test includes the test of dump_module.rb
  def test_initialize

    info = make_info["TestClassB"]
    
    assert_equals( "class", info.type )
    assert_equals( RRB::NS["TestClassB"], info.class_name )
    assert_equals( [ RRB::NS["TestClassA"], RRB::NS["Object"], RRB::NS["Kernel"] ],
                   info.ancestor_names )
    assert_equals( [ RRB::NS["TestClassA"], RRB::NS["Object"], RRB::NS["Kernel"] ],
		  info.ancestors.map{|klass| klass.class_name} )
    assert_equals( ["pub"], info.public_method_names )
    assert_equals( ["pro1", "pro2"], info.protected_method_names.sort )
    assert_equals( [], info.private_method_names )
    assert_equals( ["sing"], info.singleton_method_names )
    assert_equals( ["C1", "C3"], info.consts )
  end

  def test_has_method?
    info = make_info["TestClassB"]

    assert_equals( true, info.has_method?( "pub") )
    assert_equals( true, info.has_method?( "pubA", true ) )
    assert_equals( false, info.has_method?( "pubA", false ) )
    assert_equals( true, info.has_method?( "id" ) )
    assert_equals( false, info.has_method?( "nothing" ) ) 
  end

  def test_subclass_of?
    info = make_info

    assert_equals( true, info["TestClassB"].subclass_of?("TestClassA") )
    assert_equals( true, info["TestClassB"].subclass_of?("TestClassB") )
    assert_equals( false, info["TestClassA"].subclass_of?("TestClassB") )
    assert_equals( false, info["TestClassX"].subclass_of?("Object") )
  end

  def test_EQUAL
    info = make_info["TestClassB"]
    assert_equals( RRB::NullDumpedClassInfo.instance, RRB::NullDumpedClassInfo.instance )
    assert_equals( false, RRB::NullDumpedClassInfo.instance ==  info )
    assert_equals( false, Object.new ==  RRB::NullDumpedClassInfo.instance )
  end

  def test_superclass
    info = make_info
    assert_equals( info[RRB::NS["TestClassA"]],
                   info[RRB::NS["TestClassB"]].superclass )
    assert_equals( info[RRB::NS["TestClassA"]],
                   info[RRB::NS["TestClassC"]].superclass )
  end

  def test_resolve_const
    info = make_info
    assert_equals( RRB::NS[""],
                   info.resolve_const( RRB::NS["TestClassA"], "TestClassA" ) )
    assert_equals( RRB::NS["TestClassA"],
                   info.resolve_const( RRB::NS["TestClassA"], "C1" ) )
    assert_equals( RRB::NS["TestClassB"],
                   info.resolve_const( RRB::NS["TestClassB"], "C1" ) )
    assert_equals( RRB::NS["TestClassD"],
                   info.resolve_const( RRB::NS["TestClassD::TestClassE"], "C1" ) )
    assert_equals( nil,
                   info.resolve_const( RRB::NS["TestClassD::TestClassE"], "C3" ) )
    assert_equals( RRB::NS[""],
                   info.resolve_const( RRB::NS[""], "TestClassA" ) )
    assert_equals( nil,
                   info.resolve_const( RRB::NS[""], "C1" ) )
  end
  
  def make_info
    File.open(DUMPED_FILE_NAME) do |file|
      return RRB::DumpedInfo.get_dumped_info( file )
    end
  end
  
  def setup
    Dir.mkdir WORK_DIR
    File.open( RUBY_SCRIPT_NAME, "w" ) do |output|
      output << DUMPED_SCRIPT
      output << RRB::DUMP_MODULES_SCRIPT
    end
    
    system "ruby -r rrb_reflection #{RUBY_SCRIPT_NAME} > #{DUMPED_FILE_NAME}"    
  end
  
  def teardown
    File.delete( RUBY_SCRIPT_NAME, DUMPED_FILE_NAME )
    Dir.rmdir( WORK_DIR )
  end
end

if $0 == __FILE__
  if ARGV.size == 0
    suite = TestDumpedInfo.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      suite.add_test(TestDumpedInfo.new(testmethod))
    end
  end
  RUNIT::CUI::TestRunner.run(suite)
end
