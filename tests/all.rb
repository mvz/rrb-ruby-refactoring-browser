
Dir.glob('tests/test*.rb') do |filename|
  require filename
end

suite = RUNIT::TestSuite.new

ObjectSpace.each_object(Class) do |klass|
  if klass.superclass == RUNIT::TestCase then
    suite.add_test( klass.suite )
  end
end

if __FILE__ == $0 then
  RUNIT::CUI::TestRunner.run( suite )
end
