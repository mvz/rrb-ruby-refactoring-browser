= Ruby Refactorin Broser Embedding Manual

This document will help you to embed this tool in your IDE, editor, or etc.

== case 1. your editor is written in ruby, or ruby interpreter is embedded
If your editor can run ruby script directly
you can easily embed refactoring browser in your editor.

lib/rrb/cui_interface and rubyide_tools_rrb/rrb_plugin are sample scripts
for you.

First of all, you should load rrb library.
What you have to do is to write
  require 'rrb/rrb'
in your code.

Second, your editor ask user what script to refactor, what variable to rename,
what lines to extract as method or etc.

Third, you should make ((<RRB::Script>)) object.
Script means `a set of refactored file', and each file is represented as
((<RRB::ScriptFile>)).
Example code is following.
  files = buffers.map{|buffer| RRB::ScriptFile.new(buffer.string, buffer.path)}
  script = RRB::Script.new(files)

Fourth, you should examine that the refactoring is really available.
For example, rename local variable refactoring is not available when
new local variable already exists. You can test such condition to
call check methods.
  script.rename_local_variable?(method,old_var,new_var)

Last, you should modify files and output it.
  script.rename_local_variable(method,old_var,new_var)
  script.files.each do |file|
    next if file.new_script.nil?
    buffers.find{|buffer| buffer.path == file.path}.set_string(file.new_script)
  end

== case 2. 
If your editor cannot run ruby script directly, embedding is more difficult than
above way. Please see elisp/rrb.el, bin/rrb, bin/rrb_compinfo, 
lib/rrb/emacs_interface.rb as sample scripts.


= Ruby Refactoring Browser Embedded Reference

== RRB::Script
This class represents `a set of target ruby script files'.

When you refactor files, you call the method of this class.
You should give `method_name' as the instance of RRB::Method or RRB::ClassMethod,
`namespace' as the instance of RRB::Namespace,
and `path' and `lineno' means place at which new class/method is created.

=== class methods
--- new(files)

    Create new instance from array of script files.

--- new_from_filenames(*filenames)

    Create new instance from the name of files.

=== instance methods
--- rename_local_variable?(method_name, old_var, new_var)
--- rename_local_variable(method_name, old_var, new_var)

--- rename_instance_variable?(namespace, old_var, new_var)
--- rename_instance_variable(namespace, old_var, new_var)

--- rename_class_variable?(namespace, old_var, new_var)
--- rename_class_variable(namespace, old_var, new_var)

--- rename_global_variable?(old_var, new_var)
--- rename_global_variable(old_var, new_var)

--- rename_method_all?(old_method, new_method)
--- rename_method_all(old_method, new_method)

--- rename_method?(old_methods, new_method)
--- rename_method(old_methods, new_method)

--- extract_method?(path, new_mehtod, start_lineno, end_lineno)
--- extract_method(path, new_mehtod, start_lineno, end_lineno)

--- rename_constant?(old_const, new_const)
--- rename_constant(old_const, new_const)

--- pullup_method?(method_name, new_namespace, path, lineno)
--- pullup_method(method_name, new_namespace, path, lineno)


--- pushdown_method?(method_name, new_namespace, path, lineno)
--- pushdown_method(method_name, new_namespace, path, lineno)

--- extract_superclass?(namespace, new_class, target_classes, path, lineno)
--- extract_superclass(namespace, new_class, target_classes, path, lineno)

--- files

    Return all script files as array.

== RRB::ScriptFile
This class represents one script file.

=== class method
--- new(str, path)
      
    Create new instance whose content is ((|str|)) and whose file path is 
    ((|path|)).

=== instance method
--- path

    Return file path.

--- new_script

    Return refactored script as string. Return nil if this file is not 
    modified.

== RRB::Namespace
This class represents the name of namespace such as class or module.

=== class method
--- new(ns)
--- [ns]

    Create new instance from ((|ns|)), you can specify ((|ns|)) 
    as 'Foo::Bar' or '::Foo::Bar'.

=== instance method
--- name

    Return string of the name of namespace.

== RRB::Method
This class represent method name with namespace

=== class method
--- new(namespace, methodname)

    Create new instance.

--- [str]

    Create new instance from ((|str|)).
    If you give 'Foo::Bar#buz' as str, then return the instance of RRB::Method,
    and if you give 'Foo::Bar.buz', then return the instance of RRB::ClassMethod. 
      
=== instance method
--- name

    Return the name as string.

== RRB::ClassMethod
This class represents class method name.
--- new(namespace, classmethodname)
    
    Create new instance of classmethod.

=== instance method
--- name

    Return the name as string.

