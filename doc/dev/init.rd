= RRB(Ruby Refactoring Browser) 開発開始に関するガイド

これは、RRBを開発に参加する場合どのようにしてとりあえず動くまでもっていくか
を説明しています。

== CVSからソースをとってきたあとの準備方法
1. rrb/ripper以下を設定する
   rrb/ripper/README.jaを参照する
   rrb_ripper.soを適当な場所に置き、使えるようにする
   ripper.rb は 「require 'rrb/ripper'」でロードできる場所に置く。
   
2. Ruby 1.6の場合はstringio、pp.rb、pretty_print.rb、fileutils.rbをshimから
   とってきて、使えるように設定する。
   shimとは1.8の機能を1.6である程度実現するためのライブラリであり、
   詳しくは((<URL:http://raa.ruby-lang.org/list.rhtml?name=shim-ruby16_18>))を
   参照してください。
   
私(大林)の場合、rrb/sublib、rrb/sublib/rrb、というディレクトリを作り、
rrb/sublibに stringio.so, pp.rb, pretty_print.rb, rrb_ripper.so, fileutils.rb
を置き、rrb/sublib/rrb に ripper.rb を置いている。

== テストをする
テストを実行する場合はカレントディレクトリはrrb/でなければならない。
以下のようにして実行する

  [~/src/rrb]% ruby -Ilib -Isublib tests/all.rb

== 実行ファイルを動かしてみる
実行ファイルは bin/rrb で、以下のようにして実行できます。
  
  [~/src/rrb]% ruby -Ilib -Isublib bin/rrb rename-local-variable 'Rename#method_1' i j < samples/rename_local_variable_stream > output

  
