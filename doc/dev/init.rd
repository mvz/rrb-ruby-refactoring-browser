= RRB(Ruby Refactoring Browser) 開発開始に関するガイド

これは、RRBを開発に参加する場合どのようにしてとりあえず動くまでもっていくか
を説明しています。

== CVSからソースをとってきたあとの準備方法
1. rrb/ripper以下を設定する
   rrb/ripper/README.jaを参照する
   rrb_ripper.soを適当な場所に置き、使えるようにする
   ripper.rb は 「require 'rrb/ripper'」でロードできる場所に置く。
   
2. Ruby 1.6の場合はstringio、pp.rb、pretty_print.rbをshimからとってきて
   使えるように設定する

私(大林)の場合、rrb/sublib、rrb/sublib/rrb、というディレクトリを作り、
rrb/sublibに stringio.so, pp.rb, pretty_print.rb, rrb_ripper.so を置き、
rrb/sublib/rrb に ripper.rb を置いている。

== テストをする
テストを実行する場合はカレントディレクトリはrrb/でなければならない。
以下のようにして実行する

  [~/src/rrb]% ruby -Ilib -Isublib tests/all.rb

== 実行ファイルを動かしてみる
実行ファイルは bin/rrb で、以下のようにして実行できます。
  
  [~/src/rrb]% ruby -Ilib -Isublib bin/rrb rename-local-variable 'Rename#method_1' i j < samples/rename_local_variable_stream | lv

  
