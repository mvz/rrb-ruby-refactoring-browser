require 'ripper.so'

class R < RRB::Ripper
  def method_missing( mid, *args )
    puts mid
    args[0]
  end
  undef :warn
end

fname = (ARGV[0] || 'test/src.rb')
R.new(File.read(fname), fname, 1).parse
