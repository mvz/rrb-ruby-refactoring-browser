require 'fox'
require 'fox/responder'
require 'open3'

module FreeRIDE
  module RRB
    include Fox

    IO_SPLITTER = "\C-a"
    IO_TERMINATOR = '-- END --'
    ID_OK = enum(FXDialogBox::ID_LAST, 1)[0]
    
    class RRB

      extend FreeBASE::StandardPlugin
      def RRB.start(plugin)
        @@plugin = plugin

        register_to_command
        register_to_menu

        # Insert the command into the Tools menu
        plugin.transition(FreeBASE::RUNNING)
      end

      def RRB.register_to_command
        cmd_manager = @@plugin['/system/ui/commands'].manager
        cmd = cmd_manager.add('Refactor/RenameLocalVariable','&Rename Local Variable') do |slot|
          RenameLocalVariableDialog.new(@@plugin)
        end
        cmd = cmd_manager.add('Refactor/ExtractMethod','&Extract Method') do |slot|
          ExtractMethodDialog.new(@@plugin)
        end
        cmd = cmd_manager.add('Refactor/RenameInstanceVariable','&Rename Instance Variable') do |slot|
          RenameInstanceVariableDialog.new(@@plugin)
        end
        cmd = cmd_manager.add('Refactor/RenameClassVariable','&Rename Class Variable') do |slot|
          RenameClassVariableDialog.new(@@plugin)
        end
        cmd = cmd_manager.add('Refactor/RenameGlobalVariable','&Rename Global Variable') do |slot|
          RenameGlobalVariableDialog.new(@@plugin)
        end
        cmd = cmd_manager.add('Refactor/RenameMethod','&Rename Method') do |slot|
          RenameMethodDialog.new(@@plugin)
        end
        cmd = cmd_manager.add('Refactor/RenameConstant','&Rename Constant') do |slot|
          RenameConstantDialog.new(@@plugin)
        end
        cmd = cmd_manager.add('Refactor/PushdownMethod','&Push Down Method') do |slot|
          PushdownMethodDialog.new(@@plugin)
        end
        cmd = cmd_manager.add('Refactor/PullupMethod','&Pull Up Method') do |slot|
          PullupMethodDialog.new(@@plugin)
        end
      end

      def RRB.register_to_menu
        refactor_menu = @@plugin["/system/ui/components/MenuPane"].manager.add("Refactor_menu")
        refactor_menu.data = "Refactor"
        refactor_menu.attr_visible = true
        refactor_menu.manager.add_command("Refactor/RenameLocalVariable")
        refactor_menu.manager.add_command("Refactor/RenameInstanceVariable")
        refactor_menu.manager.add_command("Refactor/RenameClassVariable")
        refactor_menu.manager.add_command("Refactor/RenameGlobalVariable")
        refactor_menu.manager.add_command("Refactor/RenameMethod")
        refactor_menu.manager.add_command("Refactor/RenameConstant")
        refactor_menu.manager.add_command("Refactor/ExtractMethod")
        refactor_menu.manager.add_command("Refactor/PushdownMethod")
        refactor_menu.manager.add_command("Refactor/PullupMethod")

        # Things will be more better if menubar.manager#menuPanes exists...
        menus = []
        menubar = @@plugin["/system/ui/components/MenuBar/1"]
        menubar.each_slot do |slot|
          menus << slot.data
        end
        menus = menus[0..1] + [refactor_menu.path] + menus[2..-1]
        menubar.manager.menuPanes = menus
      end
    end


    def self.gather_scripts(plugin)
      buffer = ''
      editpane = plugin['/system/ui/components/EditPane']

      editpane.each_slot do |file|
        buffer += file.data
        buffer += IO_SPLITTER
        buffer += file['actions/get_text'].invoke()
        buffer += IO_SPLITTER
      end
      buffer += IO_TERMINATOR
      buffer += IO_SPLITTER

      return buffer
    end

    def self.rewrite_scripts(plugin, buffer)
      editpane = plugin['/system/ui/components/EditPane']
      input = StringIO.new(buffer)
      loop do	
	path = input.gets( IO_SPLITTER ).chop
	break if path == IO_TERMINATOR
	content = input.gets( IO_SPLITTER ).chop
        target_file = editpane.each_slot do |file|
          if file.data == path
            ext_object = file['actions/get_ext_object'].invoke()
            ext_object.set_text(content)
          end
        end
      end
    end

    def self.run_process(buffer, command, *args)
      args.map! do |arg|
        if arg.kind_of?(String)
          arg.gsub(/(\$.*?)/) {|str| "'" + $1 + "'"} || arg
        else
          arg.to_s
        end
      end

      result = ''
      Open3.popen3("#{command} #{args.join(' ')} --stdin-stdout") do |stdin, stdout, stderr|
        stdin.write(buffer)
        result = stdout.read()
        err = stderr.read()
        if err != ""
          p err
          raise StandardError
        end
      end
      return result
    end

    def self.do_refactoring(buffer, *args)
      run_process(buffer, "rrb", *args)
    end

    def self.run_compinfo(buffer, *args)
      run_process(buffer, "rrb_compinfo", *args)
    end

    def self.run_default_value(buffer, *args)
      run_process(buffer, "rrb_default_value", *args)
    end

    class RefactorDialog < FXDialogBox
      include Fox
      include Responder
 
      def initialize(plugin, title)
        owner = plugin["/system/ui/fox/FXMainWindow"].data
        super(owner, title, DECOR_TITLE|DECOR_BORDER|DECOR_CLOSE)

        @plugin = plugin
        @app = plugin["/system/ui/fox/FXApp"].data
        @buffer = FreeRIDE::RRB.gather_scripts(@plugin)
        @current_pane = plugin['/system/ui/current/EditPane']
        @filename = @current_pane.data
        @cursor_line = @current_pane['actions/get_cursor_line'].invoke

        FXMAPFUNC(SEL_COMMAND, ID_OK, :onCmdOK)
        FXMAPFUNC(SEL_COMMAND, ID_CANCEL, :onCmdCancel)

        hfr_buttons = FXHorizontalFrame.new(self, LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT)
        cmd_cancel = FXButton.new(hfr_buttons, "&Cancel", nil, self, ID_CANCEL, FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_RIGHT)
        cmd_ok = FXButton.new(hfr_buttons, "&OK", nil, self, ID_OK, FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_RIGHT)
        FXHorizontalSeparator.new(self, LAYOUT_SIDE_BOTTOM|SEPARATOR_GROOVE|LAYOUT_FILL_X)

      end
      
      def do_refactoring
      end

      def onCmdOK(sender, sel, ptr)
        begin
          result = do_refactoring
          FreeRIDE::RRB.rewrite_scripts(@plugin, result)
        rescue
        ensure
          onCmdCancel(sender, sel, ptr)
        end
      end
      
      def onCmdCancel(sender, sel, ptr)
        @app.stopModal(self)
        self.destroy
      end
      
    end

    class RenameDialog < RefactorDialog
      def initialize(plugin, title)
        super(plugin, title)
        txt_field = FXHorizontalFrame.new(self, LAYOUT_FILL_X)
        FXLabel.new(txt_field, "Enter new name: ", nil, JUSTIFY_LEFT|LAYOUT_CENTER_Y)
        @txt_new_variable = FXTextField.new(txt_field, 12, nil, 0, (FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_CENTER_Y))
        @txt_new_variable.setFocus
        
        @old_value =  get_word_from_position
        @txt_new_variable.text = @old_value

        self.create
        self.show(PLACEMENT_OWNER)
        @app.runModalFor(self)
      end

      def get_word_from_position
        ext_obj = @current_pane['actions/get_ext_object'].invoke
        current_pos = ext_obj.current_pos
        text_length = ext_obj.get_text_length
        buffer = ext_obj.get_text(text_length)

        left = current_pos
        loop do
          break if left == 0 || buffer[left - 1, 1] =~ /\s/
          left -= 1 
        end

        right = current_pos - 1
        loop do
          break if right == text_length || buffer[right + 1, 1] =~ /\s/
          right += 1 
        end
        buffer[left..right]
      end
     

    end

    class RenameLocalVariableDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Local Variable")
      end

      def do_refactoring
        method = FreeRIDE::RRB.run_default_value(@buffer, @filename, @cursor_line, "--method")
        result = FreeRIDE::RRB.do_refactoring(@buffer, "--rename-local-variable #{method} #{@old_value} #{@txt_new_variable.text}")
        return result
      end
    end

    class RenameInstanceVariableDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Instance Variable")
      end

      def do_refactoring
        namespace = FreeRIDE::RRB.run_default_value(@buffer, @filename, @cursor_line, "--class")
        result = FreeRIDE::RRB.do_refactoring(@buffer, "--rename-instance-variable #{namespace} #{@old_value} #{@txt_new_variable.text}")
        return result
      end
    end

    class RenameClassVariableDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Class Variable")
      end

      def do_refactoring
        namespace = FreeRIDE::RRB.run_default_value(@buffer, @filename, @cursor_line, "--class")
        result = FreeRIDE::RRB.do_refactoring(@buffer, "--rename-class-variable #{namespace} #{@old_value} #{@txt_new_variable.text}")
        return result
      end
    end

    class RenameGlobalVariableDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Global Variable")
      end

      def do_refactoring
        result = FreeRIDE::RRB.do_refactoring(@buffer, "--rename-global-variable #{@old_value} #{@txt_new_variable.text}")
        return result
      end
    end

    class RenameMethodDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Constant")
      end

      def do_refactoring
        namespace = FreeRIDE::RRB.run_default_value(@buffer, @filename, @cursor_line, "--class")
        result = FreeRIDE::RRB.do_refactoring(@buffer, "--rename-method #{namespace} #{@old_value} #{@txt_new_variable.text}")
        return result
      end
    end

    class RenameConstantDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Constant")
      end

      def do_refactoring
        namespace = FreeRIDE::RRB.run_default_value(@buffer, @filename, @cursor_line, "--class")
        result = FreeRIDE::RRB.do_refactoring(@buffer, "--rename-constant #{namespace + '::' + @old_value} #{@txt_new_variable.text}")
        return result
      end
    end



    class ExtractMethodDialog < RefactorDialog
      def initialize(plugin)
        super(plugin, "Extract Method")

        hfr_txt_field = FXHorizontalFrame.new(self, LAYOUT_FILL_X)
        FXLabel.new(hfr_txt_field, "Method name: ", nil, JUSTIFY_LEFT|LAYOUT_CENTER_Y)
        @txt_new_method = FXTextField.new(hfr_txt_field, 12, nil, 0, (FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_CENTER_Y))
        @txt_new_method.setFocus
        
        self.create
        self.show(PLACEMENT_OWNER)
        @app.runModalFor(self)
      end

      def do_refactoring()
        ext_obj = @current_pane['actions/get_ext_object'].invoke
        start_line =  ext_obj.line_from_position(ext_obj.selection_start) + 1
        end_line =  ext_obj.line_from_position(ext_obj.selection_end) + 1

        
        result = FreeRIDE::RRB.do_refactoring(@buffer, "--extract-method #{@filename} #{@txt_new_method.text} #{start_line} #{end_line}")

        return result
      end
      
    end

    class MoveMethodDialog < RefactorDialog
      def initialize(plugin, text)
        super(plugin, "Extract Method")        

        hfr_destination = FXHorizontalFrame.new(self, LAYOUT_FILL_X)

        begin
          result = FreeRIDE::RRB::run_compinfo(@buffer, "--classes")
        rescue
          return
        end
        candidates = result.split(',')

        FXLabel.new(hfr_destination, "Select Destination class: ", nil, JUSTIFY_LEFT|LAYOUT_CENTER_Y)
        @cmb_destination = FXComboBox.new(hfr_destination,candidates.size,candidates.size,nil,0,COMBOBOX_INSERT_FIRST|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X)
        candidates.each do |candidate|
          @cmb_destination.appendItem(candidate)
        end

        self.create
        self.show(PLACEMENT_OWNER)
        @app.runModalFor(self)

      end
    end
    
    class PushdownMethodDialog < MoveMethodDialog
      def initialize(plugin)
        super(plugin, "Push Down Method")
      end

      def do_refactoring
        method = FreeRIDE::RRB.run_default_value(@buffer, @filename, @cursor_line, "--method")
        result = FreeRIDE::RRB.do_refactoring(@buffer, "--pushdown-method #{method} #{@cmb_destination.text} #{@filename} #{@cursor_line}")

        return result
      end
    end

    class PullupMethodDialog < MoveMethodDialog
      def initialize(plugin)
        super(plugin, "Pull Up Method")
      end

      def do_refactoring
        method = FreeRIDE::RRB.run_default_value(@buffer, @filename, @cursor_line, "--method")
        result = FreeRIDE::RRB.do_refactoring(@buffer, "--pullup-method #{method} #{@cmb_destination.text} #{@filename} #{@cursor_line}")
        return result
      end
    end

    
  end
end

