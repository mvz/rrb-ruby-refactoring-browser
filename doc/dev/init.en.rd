= Installation Instructions

= Setup
Providing that you install RRB on '~/rrb/'.
(1) Extract source from tarball or CVS.
(2) Setup rrb/ripper
      Please see '~/rrb/ripper/README'.
      After compiling ripper, you should copy ripper.rb to ~/rrb/lib/rrb.
(3) Setup rrb/reflection
      'cd ~/rrb/reflection' and `ruby extconf.rb; make'
(4) Install shim if you use ruby 1.6.x
      ((<URL:http://raa.ruby-lang.org/list.rhtml?name=shim-ruby16_18>))
(5) Setup Environment variable
      Set RUBYLIB environment variable. 
      you should add "~/rrb/lib:~/rrb/reflection:~/rrb/ripper"
      to RUBYLIB.

= Run test
`cd ~/rrb/' and 'ruby tests/all.rb'

= Run on emacs
(1) Add "~/rrb/bin" to PATH
(2) Add (setq load-path (cons "~/rrb/elisp" load-path)) to .emacs
(3) Run emacs and load rrb by `M-x load-library'.
(4) M-x rrb-rename-local-variable or call other function.
