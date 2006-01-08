require 'rbconfig'
require 'tmpdir'

module RRB

  CONF = {
    'WORK_DIR_BASENAME' => File.join(Dir.tmpdir, "rrb_work"),
    'TAB_WIDTH' => 8,
    'INDENT_LEVEL' => 2,
    'RUBY_OPTS' => "",
    'RUBY_COMMAND' => Config::CONFIG['ruby_install_name']
  }
  
  begin
    load File.join( ENV["HOME"], ".rrbrc" )
  rescue LoadError
  end
  
  WORK_DIR_BASENAME = CONF["WORK_DIR_BASENAME"]
  TAB_WIDTH = CONF["TAB_WIDTH"]
  INDENT_LEVEL = CONF["INDENT_LEVEL"]
  RUBY_OPTS = CONF["RUBY_OPTS"]
  RUBY_COMMAND = CONF["RUBY_COMMAND"]
end
