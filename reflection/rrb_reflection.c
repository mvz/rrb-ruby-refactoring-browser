#include "ruby.h"

#if RUBY_VERSION_CODE < 170
static VALUE mod_constants_not_inherited_too(VALUE mod)
{
  return rb_mod_const_at( mod, rb_ary_new() );
}
#else
static VALUE mod_constants_not_inherited_too(VALUE mod)
{
  return rb_const_list( rb_mod_const_at(mod, 0) );
}
#endif

void Init_rrb_reflection()
{  
  rb_define_method( rb_cModule, "constants_not_inherited_too",
		    mod_constants_not_inherited_too, 0 );
}
