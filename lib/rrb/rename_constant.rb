require 'rrb/script'
require 'pp'

module RRB

  module ConstResolver
    def resolve_const(dumped_info,ns,const)
      return const if const[0,2]=="::"
      
      defined_classes = []
      dumped_info.each do |klass|
        if klass.consts.include?(const.split('::')[0])
          defined_classes <<  klass.class_name
        end
      end
        
      #nest
      a = ns.split('::')
      (a.size).downto(1) do |n|
        klass = a[0,n].join('::')
        return '::'+klass+'::'+const if defined_classes.include? klass
      end

      #inherit
      unless ns==""
        dumped_info[ns].ancestor_names.each do |name|
          next if name=="Object"
          return '::'+name+'::'+const if defined_classes.include? name
        end
      end

      #toplevel
      if dumped_info["Object"].consts.include?(const.split('::')[0])
        return '::'+const
      end

      #error...
      puts "can't resolve '#{const}' in '#{ns}'"
      return nil
    end

    def class_of(constname)
      if constname[0,2]=="::"
        ret = constname.split('::')[1..-2].join('::')
      else
        ret = constname.split('::')[0..-2].join('::')
      end
      
      if ret==""
        "Object"
      else
        ret
      end
    end
    
  end
  
  class RenameConstantVisitor < Visitor
    include ConstResolver
    
    def initialize(dumped_info, old_const, new_const_body)
      if old_const[0,2] != '::' then
        @old_const = '::' + old_const
      else
        @old_const = old_const
      end
      @old_const_body = old_const.split("::")[-1]
      @new_const_body = new_const_body
      @dumped_info = dumped_info
      @result = []
    end
    
    attr_reader :result
    
    def visit_node(namespace, node)
      ns = namespace.str
      #if node isn't method definition..
      if ModuleNode === node || SingletonClassNode === node
        ns << '::' unless namespace.str==""
        ns << node.name_id.name
      end
      
      node.consts.each do |constinfo|
        next if constinfo.elements_id[-1].name != @old_const_body
        
        if constinfo.toplevel?
          used_const = constinfo.name
        else
          used_const = resolve_const(@dumped_info, ns, constinfo.name)
        end
        
        if used_const == @old_const then
          id = constinfo.elements_id[-1]
          @result << Replacer.new(id.lineno, id.pointer,
                                    @old_const_body, @new_const_body)
        end
      end
    end

    def visit_class(namespace, node)
      if resolve_const(@dumped_info, namespace.str, node.name_id.name) == @old_const
        @result << Replacer.new(node.name_id.lineno, node.name_id.pointer,
                                @old_const_body, @new_const_body)
      end
    end

  end

  class RenameConstantCheckVisitor < Visitor
    include ConstResolver
    
    def initialize(dumped_info, old_const, new_const_body)
      if old_const[0,2] != '::' then
        @old_const = '::' + old_const
      else
        @old_const = old_const
      end
      @old_const_body = old_const.split('::')[-1]
      @new_const_body = new_const_body
      @dumped_info = dumped_info
      @result = true

      #if new const is already defined...
      klass = @old_const.split('::')[0..-2].join('::')
      klass = Object if klass==""
      if dumped_info[klass].consts.include?(@new_const_body)
        @result = false
      end
    end

    attr_reader :result

    def visit_node( namespace, node )
      return if @result==false
      
      ns = namespace.str

      #if node isn't method definition...
      if ModuleNode === node || SingletonClassNode === node
        ns << '::' unless namespace.str==""
        ns << node.name_id.name
      end

      node.consts.each do |constinfo|
        next if constinfo.elements_id[-1].name != @old_const_body

        if constinfo.toplevel?
          used_const = constinfo.name
        else
          used_const = resolve_const(@dumped_info, ns, constinfo.name)
        end
        
        if used_const == @old_const then
          new_const = class_of(constinfo.name) + "::" + @new_const_body
          new_const = resolve_const(@dumped_info, ns, new_const)

          return if new_const==nil
          if @dumped_info[class_of(new_const)].consts.include?(@new_const_body)
            @result = false  #already exists
          end
        end
      end
    end
    
    def visit_class(namespace, node)
      if resolve_const(@dumped_info, namespace.str, node.name_id.name) == @old_const
        new_const = resolve_const(@dumped_info, namespace.str, @new_const_body)
        if @dumped_info[class_of(new_const)].consts.include?(@new_const_body)
          @result = false
        end
      end
    end

  end

  class ScriptFile

    def rename_constant(dumped_info, old_const, new_const)
      visitor = RenameConstantVisitor.new(dumped_info, old_const, new_const)
      @tree.accept(visitor)
      @new_script = RRB.replace_str(@input, visitor.result)
    end

    def rename_constant?(dumped_info, old_const, new_const)
      return false unless RRB.valid_const_var?(new_const)
      visitor = RenameConstantCheckVisitor.new(dumped_info, old_const, new_const)
      @tree.accept(visitor)
      return visitor.result
    end

  end

  class Script

    def rename_constant(old_const, new_const)
      @files.each do |scriptfile|
        scriptfile.rename_constant(get_dumped_info, old_const, new_const)
      end
    end

    def rename_constant?(old_const, new_const)
      @files.each do |scriptfile|
        if not scriptfile.rename_constant?(get_dumped_info, old_const, new_const)
          return false
        end
        return true
      end
    end

  end

end
      
                        
