require 'mkmf'
$objs = %w( ripper.o )
$CFLAGS += " -DRIPPER -DRRB_RIPPER"
create_makefile 'rrb_ripper'
