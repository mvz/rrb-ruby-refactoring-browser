#!/usr/bin/env ruby
require 'getoptlong'
require 'ripper/genast'

def usage(result)
  puts "ruby ast.rb [-h|--help] [e|-exp expression] file"
  puts "-h   --help        display this message"
  puts "-e   --exp         compile expression rather than a file"
end

def main
  opts = GetoptLong.new(["--exp", "-e", GetoptLong::REQUIRED_ARGUMENT],
                        ["--help", "-h", GetoptLong::NO_ARGUMENT])

  opts.each do |opt, arg|
    if opt == "--exp"
      $OPT_e = arg
    elsif opt == "--help"
      $OPT_help = true
    end
  end
    
  usage 0 if $OPT_help
  
  if $OPT_e
    ast = GenAst.new($OPT_e)
  else
    ast = GenAst.new(ARGF)
  end
  ast.parse

  puts "BEGIN: #{ast.begin.inspect}" unless ast.begin.empty?
  puts "CODE:" 
  puts ast.code.map {|c| c.to_s}.join("\n")
  puts "END: #{ast.end.inspect}" unless ast.end.empty?
end

if __FILE__ == $0
  main
end
