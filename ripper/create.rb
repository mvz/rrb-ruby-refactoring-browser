
SOURCE_FILE = 'ripper.y'
INTERF_FILE = 'ripper.rb'
INTERF_INPUT = 'in.ripper.rb'
DISPID_FILE = 'dispids'


list = nil
File.open( SOURCE_FILE ) {|f|
    list = f.collect {|line|
        m = /dispatch\d\((\w+)/.match(line) and m[1]
    }.compact.push('scan').push('OP').sort.uniq
}


File.open( DISPID_FILE + '.h', 'w' ) {|f|
    list.each do |i|
      f.puts "static ID rip_id_#{i};"
    end
}

File.open( DISPID_FILE + '.c', 'w' ) {|f|
    f.print <<'-'
static void
rip_init_dispatch_ids()
{
-
    list.each do |i|
      f.puts %Q!    rip_id_#{i} =\trb_intern("on__#{i}");!
    end
    ;f.print <<'-'
}
-
}

File.open( INTERF_FILE, 'w' ) {|f|
File.foreach( INTERF_INPUT ) do |line|
  unless m = /\A\#include (\w+)/.match(line) then
    f.print line
  else
    case m[1]
    when /handlers/
      list.each do |name|
        f.print <<METHOD
    def on__#{name}( *args )
      nil
    end
METHOD
      end
    else
      raise 'unknown arg for #include'
    end
  end
end
}
