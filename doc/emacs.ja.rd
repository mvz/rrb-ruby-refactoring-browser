= Ruby Refactoring Browser Emacs Interface Manual
== インストール
README.jaを見てください。
== 基本
M-x 関数名 でリファクタリングを実行できます。指定するパラメータは
だいたいタブキーによる補完が可能です。それができないのは
以下のリストの「指定するパラメータ」の中で、「新しい」と付いている
ものです。

また、M-x rrb-undoで直前のリファクタリングをもとに戻せます。
== リファクタリングのリスト
=== ローカル変数名の変更
==== emacsでの関数名
rrb-rename-local-variable
==== 機能 
ローカル変数の名前を変更する。
==== 指定するパラメータ
* 変更するローカル変数のあるメソッド( ClassName#method_name 形式 )
* 元のローカル変数名
* 新しい変数名
==== 注意
とくになし
=== インスタンス変数名の変更
==== emacsでの関数名
rrb-rename-instance-variable
==== 機能
インスタンス変数名を変更する。
==== 指定するパラメータ
* 変更するインスタンス変数の属するクラス
* 元のインスタンス変数名
* 新しい変数名
==== 注意
指定したクラスのスーパークラスで同じ名前のインスタンス変数を使って
いる場合は、そのクラスのサブクラスすべてで変数が変更される。
=== クラス変数名の変更
==== emacsでの関数名
rrb-rename-class-variable
==== 機能
クラス変数名を変更する。
==== 指定するパラメータ
* 変更するクラス変数の属するクラス
* 元のクラス変数名
* 新しい変数名
==== 注意
指定したクラスのスーパークラスで同じ名前のクラス変数を使って
いる場合は、そのクラスのサブクラスすべてで変数が変更される。
=== グローバル変数名の変更
==== emacsでの関数名
rrb-rename-global-variable
==== 機能
グローバル変数名を変更する。
==== 指定するパラメータ
* 元のグローバル変数名
* 新しい変数名
==== 注意
とくになし
=== 定数名の変更
==== emacsでの関数名
rrb-rename-constant
==== 機能
定数の名前を変更する。
==== 指定するパラメータ
* 元の定数名 (::A::B::Cという形式で)
* 新しい名前
==== 注意
とくになし
=== クラス名の変更
==== emacsでの関数名
rrb-rename-class
==== 機能
クラスの名前を変更する。
==== 指定するパラメータ
* 元のクラス名 (::A::B::Cという形式で)
* 新しい名前
==== 注意
定数名の変更と実質的に同じ、補完にクラス名しか現われないことのみ異なる。
=== メソッド名の変更
==== emacsでの関数名
rrb-rename-method
==== 機能
メソッド名を変更する。
==== 指定するパラメータ
* 変更するメソッドの属するクラス、複数指定できる。
* 元のメソッド名
* 新しいメソッド名
==== 注意
指定したクラスのスーパークラスで指定したメソッドを定義、もしくは呼び出し
していれば、そのクラスも影響を受ける。

基本的にメソッド呼び出しのほうはself省略のメソッド呼びだしのみ変更
される。
=== メソッド名の大域的変更
==== emacsでの関数名
rrb-rename-method-all
==== 機能
メソッド名を変更する。
==== 指定するパラメータ
* 元のメソッド名
* 新しいメソッド名
==== 注意
指定した名前のメソッドの定義および呼び出しをすべて置換する。
=== メソッドの抽出
==== emacsでの関数名
rrb-extract-method
==== 機能
あるメソッドの一部を別のメソッドとして抽出する
==== 指定するパラメータ
* 抽出する範囲( C-SPCでマークしたところから現在のカーソルのあるところまで )
* 新しいメソッドの名前
==== 注意
抽出する範囲は行単位でしか見ないので、指定したリージョンは適当に行単位
に解釈される。

抽出したメソッドは抽出されたメソッドのすぐ上に置かれる。
=== スーパークラスの抽出
==== emacsでの関数名
rrb-extract-superclass
==== 機能
共通したスーパークラスを持つ複数のクラスを指定して、それらの新しい
スーパークラスを作る。
==== 指定するパラメータ
* 新しいクラスを置く場所(カーソルのある行)
* 新しいクラスの属するネームスペース
* 新しいクラスの名前
* 新たなクラスのサブクラスとなるクラス(複数指定可能)
==== 注意
カーソルのある行は空白行でなくてはならない。
=== メソッドのスーパークラスへの引き上げ
==== emacsでの関数名
rrb-pullup-method
==== 機能
指定したメソッドを指定したスーパークラスへ移動する
==== 指定するパラメータ
* 新しいメソッドを置く場所(カーソルのある行)
* 移動するメソッド名(ClassName#method_nameという形式で)
* 移動先のクラス
==== 注意
カーソルのある行は空白行でなくてはならない。
=== メソッドのサブクラスへの引き下げ
==== emacsでの関数名
rrb-pullup-method
==== 機能
指定したメソッドを指定したサブクラスへ移動する
==== 指定するパラメータ
* 新しいメソッドを置く場所(カーソルのある行)
* 移動するメソッド名(ClassName#method_nameという形式で)
* 移動先のクラス
==== 注意
カーソルのある行は空白行でなくてはならない。

