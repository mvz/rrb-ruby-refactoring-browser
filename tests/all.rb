require 'runit/testcase'
require 'runit/cui/testrunner'

if __FILE__ == $0 then
  Dir.glob('tests/test*.rb') do |filename|
    require filename
  end

  suite = RUNIT::TestSuite.new

  ObjectSpace.each_object(Class) do |klass|
    if klass.superclass == RUNIT::TestCase then
      suite.add_test( klass.suite )
    end
  end

  RUNIT::CUI::TestRunner.run( suite )
end

if defined?(Test::Unit) && Test::Unit.respond_to?( :run= )
  Test::Unit.run=true
end
