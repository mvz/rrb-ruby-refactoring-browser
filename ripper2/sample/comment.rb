#
# extracts '#' comments from ruby scripts
#

require 'ripper'

class ExtractComments < Ripper
  def on__comment( str )
    puts str
  end
  def method_missing( mid, *args )
    args[0]
  end
end

ExtractComments.new(ARGF).parse
