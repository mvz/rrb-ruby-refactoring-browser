= Ruby Refactorin Broser Embedding Manual

このドキュメントはIDEやエディタからRuby Refactoring Browserを利用できるように
するために必要なことを書いています。

== case 1. Ruby スクリプトを直接呼べる場合
もし、対象のエディタ、IDEがRubyで書かれていたり、vimのようにRubyを組みこんだり
していて、Rubyスクリプトを直接ロードして実行できる場合、このツールの組込みは
比較的簡単です。

lib/rrb/cui_interface.rb や rubyide_tools_rrb/rrb_plugin.rb が参考になると
思います。

とりあえず、リファクタリングブラウザのインストールをしましょう。
そして以下のようにすれば組みこみができます。

まず、(Rubyで書かれた)コアライブラリをロードします。
  require 'rrb/rrb'

次に、そのユーザへの問い合わせ部を作ります。変更する変数名や、どこから
どこまでをメソッドとして抜きだすか、などを得られるようにします。

そして、((<RRB::Script>))のオブジェクトを生成します。
個々のファイルが((<RRB::ScriptFile>))の1インスタンスに対応していて、
それを集めたものが((<RRB::Script>))です。

だいたい以下のようにします。
  files = buffers.map{|buffer| RRB::ScriptFile.new(buffer.string, buffer.path)}
  script = RRB::Script.new(files)

次に、リファクタリングできるかどうかのテストをします。例えば、
rename local variableをする場合は、変更後の変数がすでに存在しないか
どうかなどをチェックします。それぞれのリファクタリングの種類に対応した
チェック用メソッドが存在します。

例です。
  script.rename_local_variable?(method,old_var,new_var)

最後に、実際にコードをリファクタリングして、その結果を出力します。

以下のような感じになるでしょう。
  script.rename_local_variable(method,old_var,new_var)
  script.files.each do |file|
    next if file.new_script.nil?
    buffers.find{|buffer| buffer.path == file.path}.set_string(file.new_script)
  end

== case 2. Ruby スクリプトを直接呼べない場合
Emacsのように、Rubyのスクリプトを直接呼べない場合は、上の方法より難しくなります。
とりあえず、 elisp/rrb.el, bin/rrb, bin/rrb_compinfo, lib/rrb/emacs_interface.rb
などを参考にして何らかの方法でRubyの処理系と通信するようにしてください。

= Ruby Refactoring Browser Embedded Reference

以下は組み込みを考えている人向けのリファレンスです。

== RRB::Script
このクラスは「リファクタリング対象となるRubyスクリプト全体」を表わします。
IDEなどでは、「プロジェクト」と呼ばれるようなものであると考えれば良いでしょう。

リファクタリングするときは、このクラスのメソッドを呼びだします。

=== class methods
--- new(files)

    新しいインスタンスを((<ScriptFile>))の配列から生成する。

--- new_from_filenames(*filenames)

    新しいインスタンスをファイル名から生成する。

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
このクラスは、「対象のファイル1個」を表します。

=== class method
--- new(str, path)
      
    ファイルの内容が ((|str|)) でパスが((|path|))であるインスタンスを
    生成します。
    
=== instance method
--- path

    ファイルのパスを返します。

--- new_script

    リファクタリングされたソースコードを返します。
    何も変更されなかった場合はnilを返します。
    
== RRB::Namespace
このクラスはクラス、モジュールといった「名前空間」を表します。

=== class method
--- new(ns)
--- [ns]

    新たなインスタンスを生成します。((|ns|))は 'Foo::Bar' や '::Foo::Bar'
    などと指定してください。

=== instance method
--- name

    そのオブジェクトが表わす「名前空間」を文字列で返します。

== RRB::Method
このクラスは名前空間の付いたメソッド名を表わします。
つまり、Foo::Bar#buz といった概念に対応しています。

=== class method
--- new(namespace, methodname)

    新たなインスタンスを生成します。
    
--- [str]

    ((|str|))から新たなインスタンスを生成します。
    'Foo::Bar#buz'と指定すれば((<RRB::Method>))のインスタンスが、
    'Foo::Bar.buz'と指定すれば((<RRB::ClassMethod>))のインスタンスが得られます。
      
=== instance method
--- name

    オブジェクトを文字列化したものを返します。

== RRB::ClassMethod
クラスメソッドを表わします。

--- new(namespace, classmethodname)
    
    新たなインスタンスを生成します。

=== instance method
--- name

    オブジェクトを文字列化したものを返します。

