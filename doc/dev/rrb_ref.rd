= RRB Refarence
RRBの開発用リファレンスです。

= モジュール関数
--- RRB.replace_str

= RRB::Script
リファクタリングの対象となるファイル群をあらわすクラス

== method
--- get_dumped_info
      リファクタリングの対象となるソースの情報をリフレクション機能を
      利用して得たものを((<RRB::DumpedInfo>))のインスタンスとして返す
      
= RRB::ScriptFile
リファクタリングの対象となる個々のファイルを表わすクラス

= RRB::DumpedInfo
リフレクションによって得られた情報を表わす。

== included modules
Enumerable

== method
--- classes_having_method( method )
      ((|method|))という名前のメソッドを持つクラス全てをDumpedClassInfoの
      配列として返す。

--- [](index)
      ((|index|))という名前のクラスの情報を((<RRB::DumpedClassInfo>))の
      インスタンスとして返す

      ((|index|))は文字列、もしくは((<RRB::Namespace>))のインスタンスで
      なければならない。 
      
--- resolve_const( namespace, const )
      定数の名前解決をする

      ((|namespace|))という名前空間上にある((|const|))という定数が
      実際にはどのネームスペースに属しているものであるのかを返す。
      
      返値は((<RRB::Namespace>))のインスタンスである。
      
= RRB::Replacer
これを使ってソースの置換をする。

== class method
--- new( lineno, pointer, before, after )
      ((|lineno|))行目、行頭から((|pointer|))番目にある((|before|))という
      文字列を((|after|))という文字列に置換するReplacerを生成する
      
--- new_from_id( id, after )
      ((|id|))で与えられた識別子( ((<RRB::IdInfo>))のインスタンス )を
      ((|after|))という名前で置換するReplacerを生成する
      
= RRB::DumpedClassInfo
リフレクションによって得られた個々のクラスの情報

== method
--- type
      "class"、もしくは"module"を返す
      
--- class_name
      クラスの名前を文字列で返す
      
--- ancestor_names
      スーパークラスの配列を返す。自分自身は含まない。
      
--- public_method_names
      パブリックメソッドの名前の文字列の配列を返す
      
--- protected_method_names
      プロテクティッドメソッドの名前の文字列の配列を返す
      
--- private_method_names
      プライベートメソッドの名前の文字列の配列を返す
      
--- singleton_method_names
      特異メソッドの名前の文字列の配列を返す
      
--- consts
      定数の名前の配列を返す
      
--- has_method?( methodname, inherited_too=true )
      そのクラスが((|methodname|))という名前のメソッドを持っているなら真を、
      なければ偽を返す。
      
      ((|inherited_too|))が真ならスーパークラスも見る。
      
--- subclass_of?(classname)
      selfが((|classname|))という名前のクラスのサブクラスであれば真を、
      なければ偽を返す。

      ((|classname|))は文字列か((<RRB::Namespace>))のインスタンス
      でなければならない
      
= RRB::NullDumpedClassInfo
((<RRB::DumpedClassInfo>))のダミーとなるオブジェクトを作るクラス
存在しないクラスの情報を取ろうとすると得られる。
NullObjectパターン&singletonパターンを参照

== class method
--- instance
      return the singleton instance
      
== method
--- type
      return "NullDumpedClass"
--- class_name
      return "NullDumpedClass"
--- ancestor_names
--- public_method_names
--- protected_method_names
--- private_method_names
--- singleton_method_names    
--- consts = consts
      return empty array
--- has_method?( methodname, inherited_too=true )
--- subclass_of?(classname)
      return false

= RRB::Node
パースした結果得られる構文木の個々のノードをあらわす。
このクラスのインスタンスは生成されず、
実際には以下のRRB::*Nodeというクラスに継承して使われている。

以下のlocal_varsなどでは同じ識別子が複数回使われていればその回数分だけ
配列にあらわれることに注意

== method
--- name_id
      そのノードに対応する「名前」(MethodNodeならメソッド名、ClassNodeなら
      クラス名)の識別子を返す
      
--- class_defs
      クラス定義ノードの配列
      
--- method_defs
      メソッド定義ノードの配列を返す
      
--- method_calls
      そのノード内のメソッド呼び出しの((<RRB::IdInfo>))の配列を返す
      
--- local_vars
      そのノード内のローカル変数の((<RRB::IdInfo>))の配列を返す
      
--- global_vars
      そのノード内のグローバル変数の((<RRB::IdInfo>))の配列を返す
      
--- instance_vars
      そのノード内のインスタンス変数の((<RRB::IdInfo>))の配列を返す
      
--- class_vars
      そのノード内のクラス変数の((<RRB::IdInfo>))の配列を返す
      
--- consts
      そのノード内の定数の((<RRB::ConstInfo>))の配列を返す
      
--- fcalls
      そのノード内のレシーバを省略したメソッド呼び出しの
      ((<RRB::IdInfo>))の配列を返す
      
--- singleton_method_defs
      特異メソッド定義ノードを返す
      
--- class_method_defs
      クラスメソッド定義ノードを返す
      
--- singleton_class_defs
      特異クラス定義ノードを返す
      
--- head_keyword
      その定義の開始予約語(class,defなど)を((<RRB::IdInfo>))で返す
      
--- tail_keyword
      その定義の終了予約語(end)を((<RRB::IdInfo>))で返す
      
--- assigned
      そのノード内のローカル変数で、代入文の左辺として使われているものを
      ((<RRB::IdInfo>))の配列で返す
      
--- attr_readers
      そのノード内でのattr_readerの引数となったシンボルに対応する
      ((<RRB::IdInfo>))の配列を返す
      
--- attr_writers
      そのノード内でのattr_writerの引数となったシンボルに対応する
      ((<RRB::IdInfo>))の配列を返す

--- attr_accessors
      そのノード内でのattr_accessorの引数となったシンボルに対応する
      ((<RRB::IdInfo>))の配列を返す

--- calls
      fcalls + method_callsを返す
      
--- name
      ノードの「名前」をIdInfo/ConstInfoで返す。
      クラス定義ならクラス名を、メソッド定義ならメソッド名をというように。

--- range
      そのノードの定義範囲(head_keywordからtail_keywordまで)を
      あらわすオブジェクトを((<RRB::SyntaxRange>))の
      インスタンスとして返す
      
= RRB::MethodNode
構文木のメソッドのノードを表わすクラス

== super class
((<RRB::Node>))

= RRB::ToplevelNode
構文木の一番上にあたるノード

== super class
((<RRB::Node>))

= RRB::ClassMethodNode
構文木のクラスメソッドのノードを表わすクラス

== super class
((<RRB::Node>))

= RRB::SingletonClassNode
構文木の特異クラス定義のノードを表わすクラス

== super class
((<RRB::Node>))

= RRB::ClassNode

構文木のクラス定義のノードを表わすクラス

== super class
((<RRB::ModuleNode>))

== method
--- superclass
      スーバークラスを示すConstInfoを返す
      
= RRB::ModuleNode
構文木のモジュール定義のノードを表わすクラス
現在は((<RRB::ClassNode>))とほとんど区別されていない

== super class
((<RRB::Node>))

= RRB::IdInfo
ソース内の個々の識別子を表わすクラス

== method
--- type
      識別子の種類を返す。以下のいずれかを返す
      * :id ローカル変数や普通のメソッド名などの識別子
      * :cvar クラス変数
      * :fid あきらかにメソッド名とわかる識別子(kind_of?,map!など)
      * :const 定数
      * :ivar インスタンス変数
      * :keyword 予約語
      * :op 演算子
      * :gvar グローバル変数
      * :symbol シンボル
      * :nil 無効をあらわす
      
--- lineno
      その識別子のある行番号
      
--- pointer
      その識別子の末尾の部分の行頭からの位置
      
--- name
      その識別子の文字列
      
--- head_pointer
      その識別子の頭の部分の行頭からの位置
= RRB::Replacer
ソースの置換一個をあらわす

== class method
--- new( lineno, pointer, before, after )
      ((|lineno|))行目、行頭から((|pointer|))番目の位置にある((|before|))と
      いう文字列を((|after|))という文字列に置き換える
      
--- new_from_id( id, after )
      ((|id|))という識別子を((|after|))におきかえる
= RRB::ConstInfo
ソース内の定数を(A::B::Cなど)表わすクラス
((<RRB::IdInfo>))の列として考えられる

== method
--- basename
      定数の名前を文字列でかえす

      ただし((<toplevel?>))が真であってもその頭に"::"という文字列は
      含まれない
      
--- name
      定数の名前を文字列でかえす

--- toplevel?
      その定数をあらわす文字列がソース上で::A::Bという形であれば真、
      A::Bという形であれば偽を返す
      
--- elements_id
      その定数を構成する個々の識別子((<RRB::IdInfo>))の配列を返す

--- body
      その定数の「本体」(A::B::CならC)に対応する識別子((<RRB::IdInfo>))
      を返す

= RRB::SyntaxRange
あるノードの定義範囲、つまり定義開始予約語(classなど)と
定義終了予約語(end)の対をあらわす。

== method
--- head
      定義開始予約語(classなど)を((<RRB::IdInfo>))のインスタンスで返す

--- tail
      定義終了予約語(end)を((<RRB::IdInfo>))のインスタンスで返す

--- contain?( range )
      ((|range|))で指定した範囲がselfの範囲の中に含まれていれば真を、
      なければ偽を返す

--- out_of?( range )
      引数で指定した範囲がselfの範囲と交わらなければ真を、交われば偽を返す
      
= RRB::NodeNamespace
Visitorで構文木をtraverseしているときに、visit_*に与えられる現在のノードの
名前空間を表わすクラス

== class method
--- new( cur_node, cur_ns )
      ((|cur_ns|))の下に((|cur_node|))をくっつけた名前空間を
      あらわすインスタンスを生成する

== method
--- name
      そのインスタンスがあらわす名前空間をあらわす文字列を返す
      
--- match?( ns )
      ((<RRB::Namespace>))との一致判定をする。
      ((|ns|))は RRB::Namespace のインスタンスである必要がある。

--- normal
      selfに対応する((<RRB::Namespace>))のインスタンスを返す
      
= RRB::Namespace
名前空間(A::Bなど)を表わすクラス

hash,eql?が定義してあるのでSetの元やHashのキーとして利用できる

== class method
--- new( ns )
--- []( ns )
      ((|ns|))があらわすインスタンスを生成する。

      引数は文字列か文字列配列でなければならない。
      
== method
--- name
      名前空間に対応する文字列を返す
      
--- ary
      名前空間に対応する配列を返す
      
--- ==
      等しいかどうかを判定する

--- chop
      一番末尾の部分を削ったものを返すメソッド
      つまり、RRB::NS["A::B::C"] に対し RRB::NS["A::B"]を返す
