#!ruby -s
#
# ripper/extconf.rb
#

require 'mkmf'

def main
  raise "Ripper requires bison to be installed.  See README for prerequisites" unless have_command('bison')
  $objs = %w( ripper.o )
  $cleanfiles.concat %w( ripper.y ripper.c ripper.output
                         dispids.[ch] lib/ripper.rb lex.c )
  $CPPFLAGS += ' -DRIPPER'
  $CPPFLAGS += ' -DRIPPER_DEBUG' if $debug
  create_makefile 'rrb_ripper'
end

def have_command( cmd )
  ENV['PATH'].split(/#{File::PATH_SEPARATOR}/).each do |path|
    return true if File.executable?("#{path}/#{cmd}")
  end
  false
end

main
