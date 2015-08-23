= Ruby Refactorin Broser Embedding Manual

$B$3$N%I%-%e%a%s%H$O(BIDE$B$d%(%G%#%?$+$i(BRuby Refactoring Browser$B$rMxMQ$G$-$k$h$&$K(B
$B$9$k$?$a$KI,MW$J$3$H$r=q$$$F$$$^$9!#(B

== case 1. Ruby $B%9%/%j%W%H$rD>@\8F$Y$k>l9g(B
$B$b$7!"BP>]$N%(%G%#%?!"(BIDE$B$,(BRuby$B$G=q$+$l$F$$$?$j!"(Bvim$B$N$h$&$K(BRuby$B$rAH$_$3$s$@$j(B
$B$7$F$$$F!"(BRuby$B%9%/%j%W%H$rD>@\%m!<%I$7$F<B9T$G$-$k>l9g!"$3$N%D!<%k$NAH9~$_$O(B
$BHf3SE*4JC1$G$9!#(B

lib/rrb/cui_interface.rb $B$d(B rubyide_tools_rrb/rrb_plugin.rb $B$,;29M$K$J$k$H(B
$B;W$$$^$9!#(B

$B$H$j$"$($:!"%j%U%!%/%?%j%s%0%V%i%&%6$N%$%s%9%H!<%k$r$7$^$7$g$&!#(B
$B$=$7$F0J2<$N$h$&$K$9$l$PAH$_$3$_$,$G$-$^$9!#(B

$B$^$:!"(B(Ruby$B$G=q$+$l$?(B)$B%3%"%i%$%V%i%j$r%m!<%I$7$^$9!#(B
  require 'rrb/rrb'

$B<!$K!"$=$N%f!<%6$X$NLd$$9g$o$;It$r:n$j$^$9!#JQ99$9$kJQ?tL>$d!"$I$3$+$i(B
$B$I$3$^$G$r%a%=%C%I$H$7$FH4$-$@$9$+!"$J$I$rF@$i$l$k$h$&$K$7$^$9!#(B

$B$=$7$F!"(B((<RRB::Script>))$B$N%*%V%8%'%/%H$r@8@.$7$^$9!#(B
$B8D!9$N%U%!%$%k$,(B((<RRB::ScriptFile>))$B$N(B1$B%$%s%9%?%s%9$KBP1~$7$F$$$F!"(B
$B$=$l$r=8$a$?$b$N$,(B((<RRB::Script>))$B$G$9!#(B

$B$@$$$?$$0J2<$N$h$&$K$7$^$9!#(B
  files = buffers.map{|buffer| RRB::ScriptFile.new(buffer.string, buffer.path)}
  script = RRB::Script.new(files)

$B<!$K!"%j%U%!%/%?%j%s%0$G$-$k$+$I$&$+$N%F%9%H$r$7$^$9!#Nc$($P!"(B
rename local variable$B$r$9$k>l9g$O!"JQ998e$NJQ?t$,$9$G$KB8:_$7$J$$$+(B
$B$I$&$+$J$I$r%A%'%C%/$7$^$9!#$=$l$>$l$N%j%U%!%/%?%j%s%0$N<oN`$KBP1~$7$?(B
$B%A%'%C%/MQ%a%=%C%I$,B8:_$7$^$9!#(B

$BNc$G$9!#(B
  script.rename_local_variable?(method,old_var,new_var)

$B:G8e$K!"<B:]$K%3!<%I$r%j%U%!%/%?%j%s%0$7$F!"$=$N7k2L$r=PNO$7$^$9!#(B

$B0J2<$N$h$&$J46$8$K$J$k$G$7$g$&!#(B
  script.rename_local_variable(method,old_var,new_var)
  script.files.each do |file|
    next if file.new_script.nil?
    buffers.find{|buffer| buffer.path == file.path}.set_string(file.new_script)
  end

== case 2. Ruby $B%9%/%j%W%H$rD>@\8F$Y$J$$>l9g(B
Emacs$B$N$h$&$K!"(BRuby$B$N%9%/%j%W%H$rD>@\8F$Y$J$$>l9g$O!">e$NJ}K!$h$jFq$7$/$J$j$^$9!#(B
$B$H$j$"$($:!"(B elisp/rrb.el, bin/rrb, bin/rrb_compinfo, lib/rrb/emacs_interface.rb
$B$J$I$r;29M$K$7$F2?$i$+$NJ}K!$G(BRuby$B$N=hM}7O$HDL?.$9$k$h$&$K$7$F$/$@$5$$!#(B

= Ruby Refactoring Browser Embedded Reference

$B0J2<$OAH$_9~$_$r9M$($F$$$k?M8~$1$N%j%U%!%l%s%9$G$9!#(B

== RRB::Script
$B$3$N%/%i%9$O!V%j%U%!%/%?%j%s%0BP>]$H$J$k(BRuby$B%9%/%j%W%HA4BN!W$rI=$o$7$^$9!#(B
IDE$B$J$I$G$O!"!V%W%m%8%'%/%H!W$H8F$P$l$k$h$&$J$b$N$G$"$k$H9M$($l$PNI$$$G$7$g$&!#(B

$B%j%U%!%/%?%j%s%0$9$k$H$-$O!"$3$N%/%i%9$N%a%=%C%I$r8F$S$@$7$^$9!#(B

=== class methods
--- new(files)

    $B?7$7$$%$%s%9%?%s%9$r(B((<ScriptFile>))$B$NG[Ns$+$i@8@.$9$k!#(B

--- new_from_filenames(*filenames)

    $B?7$7$$%$%s%9%?%s%9$r%U%!%$%kL>$+$i@8@.$9$k!#(B

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
$B$3$N%/%i%9$O!"!VBP>]$N%U%!%$%k(B1$B8D!W$rI=$7$^$9!#(B

=== class method
--- new(str, path)
      
    $B%U%!%$%k$NFbMF$,(B ((|str|)) $B$G%Q%9$,(B((|path|))$B$G$"$k%$%s%9%?%s%9$r(B
    $B@8@.$7$^$9!#(B
    
=== instance method
--- path

    $B%U%!%$%k$N%Q%9$rJV$7$^$9!#(B

--- new_script

    $B%j%U%!%/%?%j%s%0$5$l$?%=!<%9%3!<%I$rJV$7$^$9!#(B
    $B2?$bJQ99$5$l$J$+$C$?>l9g$O(Bnil$B$rJV$7$^$9!#(B
    
== RRB::Namespace
$B$3$N%/%i%9$O%/%i%9!"%b%8%e!<%k$H$$$C$?!VL>A06u4V!W$rI=$7$^$9!#(B

=== class method
--- new(ns)
--- [ns]

    $B?7$?$J%$%s%9%?%s%9$r@8@.$7$^$9!#(B((|ns|))$B$O(B 'Foo::Bar' $B$d(B '::Foo::Bar'
    $B$J$I$H;XDj$7$F$/$@$5$$!#(B

=== instance method
--- name

    $B$=$N%*%V%8%'%/%H$,I=$o$9!VL>A06u4V!W$rJ8;zNs$GJV$7$^$9!#(B

== RRB::Method
$B$3$N%/%i%9$OL>A06u4V$NIU$$$?%a%=%C%IL>$rI=$o$7$^$9!#(B
$B$D$^$j!"(BFoo::Bar#buz $B$H$$$C$?35G0$KBP1~$7$F$$$^$9!#(B

=== class method
--- new(namespace, methodname)

    $B?7$?$J%$%s%9%?%s%9$r@8@.$7$^$9!#(B
    
--- [str]

    ((|str|))$B$+$i?7$?$J%$%s%9%?%s%9$r@8@.$7$^$9!#(B
    'Foo::Bar#buz'$B$H;XDj$9$l$P(B((<RRB::Method>))$B$N%$%s%9%?%s%9$,!"(B
    'Foo::Bar.buz'$B$H;XDj$9$l$P(B((<RRB::ClassMethod>))$B$N%$%s%9%?%s%9$,F@$i$l$^$9!#(B
      
=== instance method
--- name

    $B%*%V%8%'%/%H$rJ8;zNs2=$7$?$b$N$rJV$7$^$9!#(B

== RRB::ClassMethod
$B%/%i%9%a%=%C%I$rI=$o$7$^$9!#(B

--- new(namespace, classmethodname)
    
    $B?7$?$J%$%s%9%?%s%9$r@8@.$7$^$9!#(B

=== instance method
--- name

    $B%*%V%8%'%/%H$rJ8;zNs2=$7$?$b$N$rJV$7$^$9!#(B

