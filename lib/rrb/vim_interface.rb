require 'rrb/rrb'

module RRB
  module VimInterface
    module_function
    
    def search_id(str, col)
      st = (str.rindex(/[^@$a-zA-Z0-9_]/, col) || -1) + 1
      ed = (str.index(/[^@$a-zA-Z0-9_]/, col) || str.size + 1) - 1
      str[st..ed]
    end

    def set_vim_var(var, val)
      VIM.command("let g:#{var} = \"#{val}\"")
    end

    def set_error(val)
      VimInterface.set_vim_var("RRBError", val)
    end

    def set_msg(val)
      VimInterface.set_vim_var("RRBMessage", val)
    end
    
    def rename_var(new_var)
      path = VIM::Buffer.current.name
      lineno, col = VIM::Window.current.cursor
      old_var = VimInterface.search_id(VIM::Buffer.current[lineno], col)
      
      if old_var == ""
        VimInterface.set_error("cursor should be on variable")
        return
      end

      files = Dir.glob(File.dirname(path) + '/*.rb')
      script = RRB::Script.new_from_filenames(*files)

      if RRB.valid_local_var?(old_var)
        method = script.get_method_on_cursor(path, lineno)
        unless method
          VimInterface.set_error("cursor should be in method")
          return
        end

        args = [RRB::Method[method.name], old_var, new_var]
        checker = :rename_local_var?
        refactorer = :rename_local_var
        VimInterface.set_msg("Rename #{old_var} in #{method.name} to #{new_var}")

      elsif RRB.valid_instance_var?(old_var)
        target = script.get_class_on_cursor(path, lineno)
        
        args = [target, old_var, new_var]
        checker = :rename_instance_var?
        refactorer = :rename_instance_var
        VimInterface.set_msg("Rename #{old_var} in #{target.name} to #{new_var}")
      elsif RRB.valid_class_var?(old_var)
        namespace = script.get_class_on_cursor(path, lineno)
        
        args = [namespace, old_var, new_var]
        checker = :rename_class_var?
        refactorer = :rename_class_var
        VimInterface.set_msg("Rename #{old_var} in #{namespace.name} to #{new_var}")
      elsif RRB.valid_global_var?(old_var)
        args = [old_var, new_var]
        checker = :rename_global_var?
        refactorer = :rename_global_var

        VimInterface.set_msg("Rename #{old_var} to #{new_var}")
      elsif RRB.valid_const?(old_var)
        namespace = script.get_class_on_cursor(path, lineno, false)
        true_ns = script.get_dumped_info.resolve_const(namespace, old_var)
        const = true_ns.nested(old_var).name
        args = [const, new_var]
        checker = :rename_constant?
        refactorer = :rename_constant

        VimInterface.set_msg("Rename #{const} to #{new_var}")
      else
        VimInterface.set_error("Invalid Identifier #{old_var}")
        return
      end

      unless script.__send__(checker, *args)
        VimInterface.set_error(script.error_message)
        return
      end
      
      script.__send__(refactorer, *args)
      script.result_rewrite_file
    end

    def extract_method(new_method, line1, line2)
      path = VIM::Buffer.current.name
      files = Dir.glob(File.dirname(path) + '/*.rb')
      script = RRB::Script.new_from_filenames(*files)

      args = [path, new_method, line1, line2]
      unless script.extract_method?(*args)
        VimInterface.set_error(script.error_message)
        return
      end
      
      script.extract_method(*args)
      script.result_rewrite_file
    end
  end
end
