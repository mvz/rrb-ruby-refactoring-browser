require 'fox'
require 'fox/responder'

require 'rrb/script'
require 'rrb/completion'
require 'rrb/rename_local_var'
require 'rrb/rename_instance_var'
require 'rrb/rename_class_var'
require 'rrb/rename_global_var'
require 'rrb/rename_method'
require 'rrb/rename_method_all'
require 'rrb/rename_constant'
require 'rrb/extract_method'
require 'rrb/move_method'
require 'rrb/pullup_method'
require 'rrb/pushdown_method'
require 'rrb/remove_parameter'
require 'rrb/extract_superclass'
require 'rrb/default_value'

module FreeRIDE
  module RRB
    include Fox

    ID_OK = enum(FXDialogBox::ID_LAST, 1)[0]
    
    class RRB

      extend FreeBASE::StandardPlugin
      def RRB.start(plugin)
        @@plugin = plugin

        register_command
        register_menu

        plugin.transition(FreeBASE::RUNNING)
      end

      def self.register_command
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

      def self.register_menu
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

    def self.new_script(plugin)
      editpanes = plugin['/system/ui/components/EditPane']

      script_files = []
      editpanes.each_slot do |editpane|
        path = editpane.data
        text = editpane['actions/get_text'].invoke()
        script_files << ::RRB::ScriptFile.new(text, path)
      end

      return ::RRB::Script.new(script_files)
    end

    def self.rewrite_script(plugin, script)
      editpanes = plugin['/system/ui/components/EditPane']
      script.files.each do |script_file|
        editpanes.each_slot do |editpane|
          if editpane.data == script_file.path
            ext_object = editpane['actions/get_ext_object'].invoke()
            ext_object.begin_undo_action()
            ext_object.set_text(script_file.new_script)
            ext_object.end_undo_action()
          end
        end
      end
    end

    def self.split_method_name( str )
      if str['#']
        a, b = str.split( /#/ )
        namespace = ::RRB::Namespace.new(a)
        method_name = ::RRB::Method.new(namespace, b)
        return method_name
      elsif str['.']
        a, b = str.split( '.' )
        namespace = ::RRB::Namespace.new(a)
        method_name = ::RRB::Method.new(namespace, b)
        return method_name
      end
    end

    class RefactorDialog < FXDialogBox
      include Fox
      include Responder
 
      def initialize(plugin, title)
        owner = plugin["/system/ui/fox/FXMainWindow"].data
        super(owner, title, DECOR_TITLE|DECOR_BORDER|DECOR_CLOSE)

        @plugin = plugin
        @app = plugin["/system/ui/fox/FXApp"].data
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
      
      def refactor
      end

      def onCmdOK(sender, sel, ptr)
        begin
          script = refactor
          FreeRIDE::RRB.rewrite_script(@plugin, script)
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
        
        @old_value =  get_word_on_cursor
        @txt_new_variable.text = @old_value

        self.create
        self.show(PLACEMENT_OWNER)
        @app.runModalFor(self)
      end

      def get_word_on_cursor
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

      def refactor
        script = FreeRIDE::RRB.new_script(@plugin)
        method = script.get_method_on_cursor(@filename, @cursor_line).name
        method_name = FreeRIDE::RRB.split_method_name(method)
        if script.rename_local_var?(method_name, @old_value, @txt_new_variable.text)
          script.rename_local_var(method_name, @old_value, @txt_new_variable.text)
        end
        return script
      end
    end

    class RenameInstanceVariableDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Instance Variable")
      end

      def refactor
        script = FreeRIDE::RRB.new_script(@plugin)
        namespace = script.get_class_on_cursor(@filename, @cursor_line)

        if script.rename_instance_var?(namespace, @old_value, @txt_new_variable.text)
          script.rename_instance_var(namespace, @old_value, @txt_new_variable.text)
        end
        return script
      end
    end

    class RenameClassVariableDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Class Variable")
      end

      def refactor
        script = FreeRIDE::RRB.new_script(@plugin)
        namespace = script.get_class_on_cursor(@filename, @cursor_line)

        if script.rename_class_var?(namespace, @old_value, @txt_new_variable.text)
          script.rename_class_var(namespace, @old_value, @txt_new_variable.text)
        end
        return script
      end
    end

    class RenameGlobalVariableDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Global Variable")
      end

      def refactor
        script = FreeRIDE::RRB.new_script(@plugin)
        if script.rename_global_var?(@old_value, @txt_new_variable.text)
          script.rename_global_var(@old_value, @txt_new_variable.text)
        end
        return script
      end
    end

    class RenameMethodDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Constant")
      end

      def refactor
        script = FreeRIDE::RRB.new_script(@plugin)
        namespace = script.get_class_on_cursor(@filename, @cursor_line)
        old_methods = [::RRB::Method.new(namespace, @old_value)]

        if script.rename_method?(old_methods, @txt_new_variable.text)
          script.rename_method(old_methods, @txt_new_variable.text)
        end
        return script
      end
    end

    class RenameConstantDialog < RenameDialog
      def initialize(plugin)
        super(plugin, "Rename Constant")
      end

      def refactor
        script = FreeRIDE::RRB.new_script(@plugin)
        namespace = script.get_class_on_cursor(@filename, @cursor_line)
        old_const = namespace.name + '::' + @old_value

        if script.rename_constant?(old_const, @txt_new_variable.text)
          script.rename_constant(old_const, @txt_new_variable.text)
        end
        return script
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

      def refactor
        script = FreeRIDE::RRB.new_script(@plugin)

        ext_obj = @current_pane['actions/get_ext_object'].invoke
        start_line =  ext_obj.line_from_position(ext_obj.selection_start) + 1
        end_line =  ext_obj.line_from_position(ext_obj.selection_end) + 1
        new_method = @txt_new_method.text
        
        if script.extract_method?(@filename, new_method, start_line, end_line)
          script.extract_method(@filename, new_method, start_line, end_line)
        end    
        return script    
      end
    end

    class MoveMethodDialog < RefactorDialog
      def initialize(plugin, text)
        super(plugin, "Extract Method")        

        hfr_destination = FXHorizontalFrame.new(self, LAYOUT_FILL_X)

        begin
          script = FreeRIDE::RRB.new_script(@plugin)
          candidates = script.refactable_classes
        rescue
          return
        end

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

      def refactor
        script = FreeRIDE::RRB.new_script(@plugin)
        method = script.get_method_on_cursor(@filename, @cursor_line).name
        method_name = FreeRIDE::RRB::split_method_name(method)
        new_namespace = ::RRB::Namespace.new(@cmb_destination.text)

        if script.pushdown_method?(method_name, new_namespace, @filename, @cursor_line)
          script.pushdown_method(method_name, new_namespace, @filename, @cursor_line)
        end
        return script
      end
    end

    class PullupMethodDialog < MoveMethodDialog
      def initialize(plugin)
        super(plugin, "Pull Up Method")
      end

      def refactor
        script = FreeRIDE::RRB.new_script(@plugin)
        method = script.get_method_on_cursor(@filename, @cursor_line).name
        method_name = FreeRIDE::RRB::split_method_name(method)
        new_namespace = ::RRB::Namespace.new(@cmb_destination.text)

        if script.pullup_method?(method_name, new_namespace, @filename, @cursor_line)
          script.pullup_method(method_name, new_namespace, @filename, @cursor_line)
        end
        return script
      end
    end
  end
end

