#
# strip-comment.rb (not completed)
#

require 'ripper'

class CommentStripper < Ripper
  def CommentStripper.strip(f)
    new(f).parse
  end

  def on__scan(event, token)
    if event == :on__comment
      puts if /\n/ === token
    else
      print token
    end
  end

  def method_missing(*args)
    ;
  end
end

CommentStripper.strip(ARGF)


__END__
# should be:

require 'ripper'

class CommentStripper < Ripper
  def CommentStripper.strip(f)
    new(f).parse
  end

  def on_scan(event, token)
    print token unless event == :comment
  end
end

CommentStripper.strip(ARGF)
