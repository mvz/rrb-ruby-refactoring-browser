= How to start developing RRB.

= Setup
(1) Extract source from tarball or CVS.
(2) Setup rrb/ripper
      Please see rrb/ripper/README.
(3) Setup rrb/reflection
      cd rrb/reflection and `ruby extconf.rb; make'
(4) Install shim if ruby 1.6
      ((<URL:http://raa.ruby-lang.org/list.rhtml?name=shim-ruby16_18>))
(5) Setup Environment variable
      Set RUBYLIB environment variable. If you put rrb files in ~/src/rrb/,
      you should add "~/src/rrb/lib:~/src/rrb/reflection:~/src/rrb/ripper"
      to RUBYLIB.

= Run test
`cd ~/src/rrb' and `ruby tests/all.rb'

= Run on emacs
(1) Add "~/src/rrb/bin" to PATH
(2) Add (setq load-path (cons "~/src/rrb/elisp" load-path)) to .emacs
(3) Run emacs and load rrb by `M-x load-library'.
(4) M-x rrb-rename-local-variable or call other function.
