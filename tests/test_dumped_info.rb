require 'runit/testcase'
require 'runit/cui/testrunner'

require 'rrb/dumped_info.rb'

WORK_DIR = '/tmp/rrbtest'
RUBY_SCRIPT_NAME = "#{WORK_DIR}/script.rb"
DUMPED_FILE_NAME = "#{WORK_DIR}/dumped"
DUMPED_SCRIPT = <<EOS
class TestClassA
  def pubA
  end
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
end
EOS

class TestDumpedInfo < RUNIT::TestCase

  # this test includes the test of dump_module.rb
  def test_initialize

    info = nil
    File.open(DUMPED_FILE_NAME) do |file|
      info = RRB::DumpedInfo.get_dumped_info( file )["TestClassB"]
    end
    
    assert_equals( "class", info.type )
    assert_equals( "TestClassB", info.module_name )
    assert_equals( ["TestClassA","Object","Kernel"], info.ancestor_names )
    assert_equals( ["pub"], info.public_method_names )
    assert_equals( ["pro1", "pro2"], info.protected_method_names.sort )
    assert_equals( [], info.private_method_names )
    assert_equals( ["sing"], info.singleton_method_names )
  end

  def setup
    Dir.mkdir WORK_DIR
    File.open( RUBY_SCRIPT_NAME, "w" ) do |output|
      File.open( "lib/rrb/dump_modules.rb", "r" ) do |input|
	output <<  DUMPED_SCRIPT
	output << input.read
      end
    end
    
    system "ruby #{RUBY_SCRIPT_NAME} > #{DUMPED_FILE_NAME}"    
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
