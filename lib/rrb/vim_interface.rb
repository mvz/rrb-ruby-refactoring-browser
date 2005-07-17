require 'rrb/rrb'

module RRB
  module VimInterface
    module_function
    def search_id(str, col)
      st = (str.rindex(/[^a-zA-Z_]/, col) || -1) + 1
      ed = (str.index(/[^a-zA-Z_]/, col) || str.size + 1) - 1
      str[st..ed]
    end

    def set_vim_var(var, val)
      VIM.command("let g:#{var} = \"#{val}\"")
    end
    
    def rename_var(new_var)
      path = VIM::Buffer.current.name
      lineno, col = VIM::Window.current.cursor
      old_var = VimInterface.search_id(VIM::Buffer.current[lineno], col)

      if old_var == ""
        VimInterface.set_vim_var("RRBError", "cursor should be on variable")
        return
      end
      
      files = Dir.glob(File.dirname(path) + '/*.rb')
      script = RRB::Script.new_from_filenames(*files)

      method = script.get_method_on_cursor(path, lineno)
      if method == nil
        VimInterface.set_vim_var("RRBError", "cursor should be in method")
        return
      end
      
      methodname = RRB::Method[method.name]

      unless script.rename_local_var?(methodname, old_var, new_var)
        VimInterface.set_vim_var("RRBError", script.error_message)
        return
      end

      script.rename_local_var(methodname, old_var, new_var)
      VimInterface.set_vim_var("RRBMessage",
                               "Rename #{old_var} in #{method.name} to #{new_var}")
      script.result_rewrite_file
    end
  end
end
