# $Id$

require 'ripper'

class EmbeddedDocumentExtractor < Ripper
  def on__embdoc(str)
    print str
  end
  def method_missing(mid, *args)
    args[0]
  end
end

EmbeddedDocumentExtractor.new(ARGF).parse
