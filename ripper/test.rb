
require 'ripper.so'

class R < Ripper
  def method_missing( mid, *args )
    args.each {|i| i.inspect }
    args[0]
  end
end

r = R.new
fname = ARGV[0] ? ARGV[0] : 'src'
s = nil
File.open( fname ) {|f|
  s = f.read
}
r.parse s, fname
