cd ..\bin

ruby -r exerb/mkexy rrb
call exerb rrb.exy

ruby -r exerb/mkexy rrb_compinfo
call exerb rrb_compinfo.exy

ruby -r exerb/mkexy rrb_default_value
call exerb rrb_default_value.exy

ruby -r exerb/mkexy rrb_marshal
call exerb rrb_marshal.exy

cd ..\package
mkdir win32bin
copy ..\bin\*.exe win32bin
