require 'rrb/script'

module RRB
  class Script
    
    def extract_superclass?( namespace, new_class, targets )
      # check namespace exists?
      unless namespace == RRB::Namespace::Toplevel then
        return false if get_dumped_info[namespace] == NullDumpedClassInfo.instance
      end
      # check all targets exist?
      targets.each do |klass|
        return false if get_dumped_info[klass] == NullDumpedClassInfo.instance
      end
      
      # check targets have the same superclass
      superclass = get_dumped_info[targets.first].superclass
      targets.each do |klass|
        return false unless get_dumped_info[klass].superclass == superclass
      end

      # check name collision
      ns = namespace
      until ns == RRB::Namespace::Toplevel
        return false if get_dumped_info[ns].consts.include?( new_class )
        ns = ns.chop
      end
      get_dumped_info[namespace].ancestors.each do |anc|
        return false if anc.consts.include?( new_class )
      end
      
      return true
    end
    
  end
end
