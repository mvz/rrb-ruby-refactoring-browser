require 'tempfile'
require 'readline'
require 'getoptlong'
require 'curses'
require 'rrb/rrb'
require 'rrb/completion'

class String
  def trim
    split(/\s+/).find{|s| s != ""}
  end
end

module RRB
  module CUI
    USAGE = <<USG
usage: rrbcui [options] FILES..

Refactoring FILES automatically.

options:
  -w              rewrite FILES [no implementation]
  -d DIFF         output diff [default, DIFF=output.diff]
  -h              print this message and exit
  -r REFACTORING  do REFACTORING
  -o              overwrite output diff file

refactoring:
  rename-local-variable [method old-var new-var]
  rename-instance-variable [class old-var new-var]
  rename-class-variable [class old-var new-var]
  rename-global-variable [old-var new-var]
  rename-constant [old-constant new-constant]
  rename-class [old-class new-class]
  rename-method-all [old-method new-method]
  rename-method [old-methods.. new-method]
  extract-method [file begin end new-method]
  extract-superclass [new-namespace new-class target-classes.. file lineno]
  pullup-method [old-class#old-method new-class file lineno]
  pushdown-method [old-class#old-method new-class file lineno]
USG

    OPTIONS = [
      ['-d', GetoptLong::REQUIRED_ARGUMENT],
      ['-w', GetoptLong::NO_ARGUMENT],
      ['--help', '-h', GetoptLong::NO_ARGUMENT],
      ['-r', GetoptLong::REQUIRED_ARGUMENT],
      ['-o', GetoptLong::NO_ARGUMENT],
    ]

    module_function
    def print_usage
      print USAGE
      exit
    end

    def select_one(prompt, words)
      Readline.basic_word_break_characters = "\t\n\"\\'"
      Readline.completion_proc = Proc.new do |word|
        words.grep(/^#{Regexp.quote(word)}/)
      end
      Readline.readline(prompt).trim
    end

    def select_any(prompt, words)
      Readline.basic_word_break_characters = " \t\n\"\\'"
      Readline.completion_proc = Proc.new do |word|
        words.grep(/^#{Regexp.quote(word)}/)
      end
      Readline.readline(prompt).split(/\s+/)
    end
    

    # this class enables you to show file, scroll, select line
    # and select region
    class Screen
      def initialize(str)
        @str = str.split(/^/)
        @cursor = 0
        @top = 0
        @start = nil
      end

      def new_region(lineno1, lineno2)
        if lineno1 < lineno2
          lineno1+1 .. lineno2+1
        else
          lineno2+1 .. lineno1+1
        end
      end

      def select
        Curses.init_screen
        begin
          Curses.nonl
          Curses.cbreak
          Curses.noecho
          
          loop do
            draw_screen
            
            key = Curses.getch
            case key
            when ?j, ?\C-n, Curses::KEY_DOWN
              cursor_down
            when ?J
              scroll_down
            when ?k, ?\C-p, Curses::KEY_UP
              cursor_up
            when ?K
              scroll_up
            when ?\s, Curses::KEY_NPAGE
              Curses.lines.times{ scroll_down }
            when ?g
              @str.size.times{ cursor_up }
            when ?G
              @str.size.times{ cursor_down }
            when ?q
              return nil
            when ?\C-m
              yield
            end
          end
          
        ensure
          Curses.clear
          Curses.close_screen
        end
      end
      
      def select_region
        select do
          if @start
            return new_region(@start, @cursor)
          else
            @start = @cursor
          end
        end
      end

      def select_line
        select do
          return @cursor + 1
        end
      end
      
      def draw_screen
        Curses.lines.times do |i|
          break if @str[@top+i] == nil
          Curses.setpos(i, 0)
          Curses.standout if i == @start
          Curses.addstr(@str[@top+i])
          Curses.standend
        end
        Curses.setpos(@cursor - @top, 0)
        Curses.refresh
      end
      
      # scroll down the screen
      def scroll_down
        return if @str[@top+Curses.lines+1] == nil
        @top += 1
        @cursor = @top if @cursor < @top
      end
      
      def cursor_down
        return if @cursor >= @str.size - 1
        @cursor += 1
        scroll_down if @cursor - @top >= Curses.lines
      end

      # scroll up the screen
      def scroll_up
        return if @top <= 0
        @top -= 1
        @cursor = @top + Curses.lines - 1 if @cursor > @top + Curses.lines - 1
      end
      
      def cursor_up
        return if @cursor <= 0
        @cursor -= 1
        scroll_up if @cursor < @top
      end
    end
    
    class UI
      def initialize(files, diff_file)
        @script = Script.new_from_filenames(*files)
        @diff_file = diff_file
      end

      def setup_diff_file
        File.open(@diff_file, "wb"){}
      end
      
      def output_diff
        setup_diff_file
        @script.files.find_all{|sf| sf.new_script != nil}.each do |sf|
          tmp = Tempfile.new("rrbcui")
          begin
            tmp.print(sf.new_script)
            tmp.close
            system("diff -u #{sf.path} #{tmp.path} >> #{@diff_file}")
          ensure
            tmp.close(true)
          end
        end
      end

      def select_one(prompt, words)
        CUI.select_one(prompt, words)
      end

      
      def select_any(prompt, words)
        CUI.select_any(prompt, words)
      end
      
      def input_str(prompt)
        Readline.completion_proc = proc{ [] }
        Readline.readline(prompt).trim
      end
  
      def select_region(scriptfile)
        Screen.new(scriptfile.input).select_region
      end

      def select_line(scriptfile)
        Screen.new(scriptfile.input).select_line
      end
      
      def classes
        @script.refactable_classes
      end

      def files
        @script.files.map{|sf| sf.path}
      end
      
      def check_and_execute(refactoring, *args)
        unless @script.__send__("#{refactoring}?", *args)
          STDERR.print(@script.error_message, "\n")
          exit
        end
        @script.__send__(refactoring, *args)
        output_diff
      end
    end

    class RenameLocalVariable < UI
      def methods
        @script.refactable_methods.map{|method| method.name}
      end

      def vars(method)
        @script.refactable_methods.find{|m| m.name == method}.local_vars.to_a
      end

      def run
        method = select_one("Refactored method: ", methods)
        old_var = select_one("Old variable: ", vars(method))
        new_var = input_str("New variable: ")
        check_and_execute("rename_local_var", Method[method], old_var, new_var)
      end
    end
    
    class RenameInstanceVariable < UI
      def ivars(target)
        @script.refactable_classes_instance_vars.each do |classname, ivars|
          return ivars if classname == target.name
        end
        return []
      end
      
      def run
        namespace = Namespace[select_one("Refactared class: ", classes)]
        old_var = select_one("Old variable: ", ivars(namespace))
        new_var = input_str("New variable: ")
        check_and_execute("rename_instance_var", namespace, old_var, new_var)
      end
    end

    class RenameClassVariable < UI
      def cvars(target)
        @script.refactable_classes_class_vars.each do |cname, cvars|
          return cvars if cname == target.name
        end
        return []
      end

      def run
        namespace = Namespace[select_one("Refactared class: ", classes)]
        old_var = select_one("Old variable: ", cvars(namespace))
        new_var = input_str("New variable: ")
        check_and_execute("rename_class_var", namespace, old_var, new_var)
      end
    end

    class RenameGlobalVariable < UI
      def gvars
        @script.refactable_global_vars
      end

      def run
        old_var = select_one("Old variable: ", gvars)
        new_var = input_str("New variable: ")
        check_and_execute("rename_global_var", old_var, new_var)
      end
    end
    
    class ExtractMethod < UI
      def run
        path = select_one("What file?: ", files)
        region = select_region(@script.files.find{|sf| sf.path == path})
        new_method = input_str("New method: ")
        check_and_execute("extract_method", path, new_method,
                          region.begin, region.end)
      end
    end

    class ExtractSuperclass < UI
      def input_new_namespace
        result = CUI.select_one("What namespace is your new class in: ", classes)
        return Namespace::Toplevel if result == nil
        return Namespace[result]
      end
      
      def run
        path = select_one("What file new class is created?: ", files)
        lineno = select_line(@script.files.find{|sf| sf.path == path})
        namespace = input_new_namespace
        new_class = input_str("New class: ")
        targets = select_any("Targets: ", classes).map{|cls| Namespace[cls]}

        check_and_execute("extract_superclass", namespace, new_class,
                          targets, path, lineno)
      end
    end

    
    REFACTORING_MAP = {
      'rename-local-variable' => RenameLocalVariable,
      'rename-instance-variable' => RenameInstanceVariable,
      'rename-class-variable' => RenameClassVariable,
      'rename-global-variable' => RenameGlobalVariable,
      'rename-constant' => nil,
      'rename-class' => nil,
      'rename-method-all' => nil,
      'rename-method' => nil,
      'extract-method' => ExtractMethod,
      'extract-superclass' => ExtractSuperclass,
      'pullup-method' => nil,
      'pushdown-method' => nil,
    }

    REFACTORING = REFACTORING_MAP.map{|name, klass| name}
    
    # parse ARGV and do refactoring
    def execute
      print_usage if ARGV.empty?

      diff_file = 'output.diff'
      refactoring = nil
      overwrite = false
      
      parser = GetoptLong.new
      parser.set_options( *OPTIONS )
      parser.each_option do |name, arg|
        print_usage if name == '--help'
        diff_file = arg if name == '-d'
        refactoring = arg if name == '-r'
        overwrite =true if name == '-o'
      end

      if !overwrite && File.exist?(diff_file)
        STDERR.print "ERROR: #{diff_file} exists\n"
        exit 1
      end
      
      refactoring = select_one("Refactoring: ", REFACTORING) unless refactoring
      ui_class = REFACTORING_MAP.fetch(refactoring){ raise 'No such refactoring' }
      ui_class.new(ARGV,diff_file).run
    end

  end
end

if $0 == __FILE__
  exit if ARGV.empty?
  screen = RRB::CUI::Screen.new(File.read(ARGV[0]))
  p screen.select_region
end
