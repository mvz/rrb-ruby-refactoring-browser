# $Id$

require 'ripper.so'

class NoopFilter < Ripper
  def on__scan(event, token)
    print token
  end

  def method_missing( mid, *args )
    args[0]
  end

  def warn(*args)
  end

  def warning(*args)
  end
end

fname = (ARGV[0] || 'test/src.rb')
NoopFilter.new(File.read(fname), fname, 1).parse
