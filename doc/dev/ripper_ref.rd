= Ripper 簡易リファレンス

== 注
* ここでいう文字列とは、基本的に"、'、%q、でくくられた文字列やヒアドキュメント
  などを指す。そうでない場合はできるだけそこで注意をうながす。

* ブロックとはいわゆるイテレータである

== クラスメソッド

--- Ripper.new(*args)
--- Ripper.parse(*args)

== メソッド

--- Ripper#parse(*args)
--- Ripper#pointer

== parse中に呼びだされるメソッド
  
--- Ripper#on__AMPER
--- Ripper#on__AND
--- Ripper#on__ANDAND
--- Ripper#on__ANDAND_ASSIGN
--- Ripper#on__AND_ASSIGN
--- Ripper#on__AREF
--- Ripper#on__ASET
--- Ripper#on__ASSIGN
--- Ripper#on__ASSOC
--- Ripper#on__AT
--- Ripper#on__BACKQUOTE
--- Ripper#on__BACKSLASH
--- Ripper#on__BACK_REF
--- Ripper#on__BANG
--- Ripper#on__BEGIN
--- Ripper#on__CHAR( str )
    
    ?a などで表わされる整数リテラルがscanされたときに呼びだされる。
    要fix
    * ((|str|)) =="?"

--- Ripper#on__CMP
--- Ripper#on__CMP_ASSIGN
--- Ripper#on__COLON
--- Ripper#on__COLON2
--- Ripper#on__COLON3
--- Ripper#on__COMMA
--- Ripper#on__CONSTANT( const_id )
    * ((|const_id|))定数識別子
    
--- Ripper#on__CVAR( cvar_id )
    *  ((|cvar_id|)) クラス変数識別子

--- Ripper#on__DOLLER
--- Ripper#on__DOT
--- Ripper#on__DOT2
--- Ripper#on__DOT3
--- Ripper#on__END
--- Ripper#on__EQ
--- Ripper#on__EQQ
--- Ripper#on__FID( function_id )

    おしりに?や!などが付いている、明らかにメソッドとわかる識別子
    * ((|function_id|))識別子
    
--- Ripper#on__GEQ
--- Ripper#on__GT
--- Ripper#on__GVAR( gvar_id )
    * ((|gvar_id|)) グローバル変数識別子

--- Ripper#on__HAT
--- Ripper#on__HAT_ASSIGN
--- Ripper#on__IDENTIFIER( id )

    * ((|id|)) 識別子
    
--- Ripper#on__INTEGER_10
--- Ripper#on__INTEGER_16
--- Ripper#on__INTEGER_2
--- Ripper#on__INTEGER_8
--- Ripper#on__IVAR( ivar_id )
    * ((|ivar_id|)) インスタンス変数識別子

--- Ripper#on__KEYWORD
--- Ripper#on__LBRACE
--- Ripper#on__LBRACE_ARG
--- Ripper#on__LBRACKET
--- Ripper#on__LPAREN
--- Ripper#on__LPAREN_ARG
--- Ripper#on__LSHIFT
--- Ripper#on__LT
--- Ripper#on__MATCH
--- Ripper#on__MINUS
--- Ripper#on__MINUS_ASSIGN
--- Ripper#on__MUL
--- Ripper#on__MUL_ASSIGN
--- Ripper#on__NEQ
--- Ripper#on__NMATCH
--- Ripper#on__NTH_REF
--- Ripper#on__OR
--- Ripper#on__OROR
--- Ripper#on__OROR_ASSIGN
--- Ripper#on__OR_ASSIGN
--- Ripper#on__PERCENT
--- Ripper#on__PERCENT_ASSIGN
--- Ripper#on__PLUS
--- Ripper#on__PLUS_ASSIGN
--- Ripper#on__POW
--- Ripper#on__Q( q )
    
    ?(クエスチョンマーク)がscanされたときに呼びだされる
    * ((|q|)) =="?"

--- Ripper#on__RBRACE
--- Ripper#on__RBRACKET
--- Ripper#on__RPAREN
--- Ripper#on__RSHIFT
--- Ripper#on__RSHIFT_ASSIGN
--- Ripper#on__SEMICOLON
--- Ripper#on__SLASH
--- Ripper#on__SLASH_ASSIGN
--- Ripper#on__STAR
--- Ripper#on__SYMBEG
--- Ripper#on__TILDE
--- Ripper#on__UMINUS
--- Ripper#on__UPLUS
--- Ripper#on__add_eval_string( context, str )
    
    文字列に実行時に評価される文字列 #{...} がscanされたときに呼びだされる。
    * ((|str|)) は実行時に評価される文字列
    
--- Ripper#on__add_space( context, str )

    %w( で文字列からなる配列を生成するとき、%w( の直後にspace文字があるとき
    よびだされる
    * ((|context|))  %w( など
    * ((|str|)) %w( の直後のspace文字列。複数続けばそれがまとめて呼びだされる。
      つまり、%w(  hoge heke) というものに対しては "  " が呼びだされる
      
--- Ripper#on__add_string( context, str )

    文字列が処理されたときに呼びだされる。
    ただし、ヒアドキュメントはとりあつかいが異なる。
    また、"befere #{hoge} after"という文字列を処理すると、
    "befere "、 " after"の部分が別々に処理される。
    また、#{}の部分は別口で(on__add_eval_stringなどで)処理される
    
--- Ripper#on__add_word( context, str )

    %w( で文字列からなる配列を生成するとき、一つ一つの文字列ごとに
    呼びだされる。
    
--- Ripper#on__alias
--- Ripper#on__ambiguous_argument
--- Ripper#on__and
--- Ripper#on__aref
--- Ripper#on__argadd
--- Ripper#on__argadd_args
--- Ripper#on__argadd_assocs
--- Ripper#on__argadd_block
--- Ripper#on__argadd_opt
--- Ripper#on__argadd_rest
--- Ripper#on__argadd_star
--- Ripper#on__argadd_value
--- Ripper#on__argstart
--- Ripper#on__argvoid
--- Ripper#on__assign
--- Ripper#on__assignable( var, arg )

    ((|var|))が代入の左辺として使われると判定された時に呼びだされる。
    ブロックの引数として使われたときも呼びだされる
    parse時に呼びだされる。
    * ((|var|)) 左辺(文字列そのまま)
    * ((|arg|)) メソッドのデフォルト引数として代入文が書かれたときの右辺。
      それ以外は nil となる
    
--- Ripper#on__assoc
--- Ripper#on__assoc_add
--- Ripper#on__assoc_list
--- Ripper#on__backref_error
--- Ripper#on__begin
--- Ripper#on__begin_block
--- Ripper#on__begin_brace

    { }  ブロックが始まるときに呼びだされる。parse時に呼びだされる。
    ((<Ripper#on__end_brace>))と対になっている。

--- Ripper#on__begin_do
    
    do end ブロックが始まるときに呼びだされる。parse時に呼びだされる。
    ((<Ripper#on__end_do>))と対になっている。

--- Ripper#on__block
--- Ripper#on__blockcall( method_call, block )

    ブロック付メソッド呼びだしのときに呼びだされる。

    引数なし、かっこなし、かつレシーバなしのメソッド呼びだしで、ブロックを
    取る場合ば fcallやvarrefは呼びだされずに、これのみが呼びだされる
    ことに注意。(ripper.yを修正してこの場合はfcallを呼ぶようにした)

    * ((|method_call|)) メソッド
    * ((|block|)) ブロック

--- Ripper#on__blockvar
--- Ripper#on__break
--- Ripper#on__call( receiver, method, args )
    
    レシーバを持つメソッド呼び出し時に呼びだされる。parse時に呼びだされる。
    * ((|receiver|)) レシーバ
    * ((|method|)) メソッド名
    * ((|args|)) 引数

--- Ripper#on__call_colon
--- Ripper#on__case
--- Ripper#on__class( class_name, stmts, superclass )
    
    クラス定義時に呼ばれる。parse時に呼びだされる。
    * ((|class_name|)) クラス名
    * ((|stmts|)) 中の文
    * ((|superclass|)) スーパークラスの名前。指定されていない場合は nil。

--- Ripper#on__comment
--- Ripper#on__comment_start
--- Ripper#on__condexpr
--- Ripper#on__const_get( receiver, const_name )
    
    A::Bという形式で定数を参照したときに呼びだされる。parse時に呼びだされる。
    ::CONST_NAME という形式の場合は((<Ripper#on__toplevel_const_get>))が
    呼びだされ、これは呼びだされない。
    また、「::」なしでの定数参照は((<Ripper#on__varref>))が呼びだされ、
    これは呼びだされない。
    * ((|receiver|)) ::の左側
    * ((|const_name|)) 定数名。::の右側
    
--- Ripper#on__def( def_str, name, arglist, statements, rescue_clause )
    
    メソッド定義時に呼ばれる
    * ((|def_str|)) == "def"
    * ((|name|)) メソッド名
    * ((|arglist|)) 引数のリスト
    * ((|statements|)) メソッドの中身
    * ((|rescue_clause|)) レスキュー節
    

--- Ripper#on__defined
--- Ripper#on__else
--- Ripper#on__elsif
--- Ripper#on__embdoc( str )
    
    埋め込みドキュメントを1行scanするごとに呼ばれる
    * ((|str|)) 埋め込みドキュメントを1行の内容(改行含む)

--- Ripper#on__embdoc_begin( str )

    埋め込みドキュメントのscan開始時に呼ばれる
    * ((|str|)) == "=begin"

--- Ripper#on__embdoc_begin_label( str )
    
    埋め込みドキュメントのscan開始時、on__embdoc_beginの後に呼びだされる
    * ((|str|)) =beginに続く文字列(not Ruby's String)

--- Ripper#on__embdoc_end

    埋め込みドキュメントのscan終了時に呼ばれる
    * ((|str|)) == "=end"

--- Ripper#on__embdoc_end_label

    埋め込みドキュメントのscan終了時、on__embdoc_endの後に呼びだされる
    * ((|str|)) =endに続く文字列(not Ruby's String)

--- Ripper#on__end_brace

    { }  ブロックが終わるに呼びだされる。parse時に呼びだされる。
    ((<Ripper#on__begin_brace>))と対になっている

--- Ripper#on__end_do

    do end  ブロックが始まるときに呼びだされる。parse時に呼びだされる。
    ((<Ripper#on__begin_do>))と対になっている

--- Ripper#on__ensure
--- Ripper#on__escaped_newline
--- Ripper#on__eval_string_begin( context, str )
    * ((|str|)) == #{
    
--- Ripper#on__eval_string_end( context, str )
    * ((|str|)) == }
    
--- Ripper#on__fcall( function, args )

    レシーバの無いメソッド呼びだしで呼ばれる。parse時に呼びだされる。
    メソッド呼びだしかローカル変数へのアクセスか区別できないときは
    呼びだされない。
    * ((|function|)) 関数名
    * ((|args|)) 引数リスト
    
--- Ripper#on__for
--- Ripper#on__here_document_end( context )
    * ((|context|)) は << など文字列開始文字列
    
--- Ripper#on__here_document_eos( context, eos )
    * ((|context|)) は << など文字列開始文字列
    * ((|eos|)) 文字列終端文字列
      
      これは以下のソースにおける "EOS" にあたる
        str == <<EOS
        hoge
         kek
        ped
        EOS
      
--- Ripper#on__here_document_eos_term( context, term )

    <<' や <<" などを使ったときに呼びだされる
        
--- Ripper#on__if
--- Ripper#on__if_mod
--- Ripper#on__ignored_newline
--- Ripper#on__infix
--- Ripper#on__is_dyna_regexp
--- Ripper#on__is_once_regexp
--- Ripper#on__is_xstring
    
    文字列のscanが終わった後、その文字列が`で囲まれたものであった場合に呼びださ
    れる。
    ヒアドキュメントの場合も呼びだされる

--- Ripper#on__lhsadd_value
--- Ripper#on__lhs_aset
    
    新設

--- Ripper#on__lhs_attrset_colon

    新設

--- Ripper#on__lhs_attrset_dot

    新設

--- Ripper#on__list_add
--- Ripper#on__list_start
--- Ripper#on__local_count( arg_name )
    
    おそらく、代入されない可能性のあるlocal変数を判定する
    ために存在するものと思われる。
    そのわりには &block でこれが処理されていないのは謎ではある。
    修正して &block でも呼びだされるようにした
    * ((|arg_name|)) 仮引数の名前
    以下の場合に呼びだされる。parserからもlexerからも。
    * メソッドの普通の仮引数一つ一つについて(parser)
    * メソッドの配列化仮引数について(parser)
    * $~ が scan されるごとに(lexer)
    
--- Ripper#on__local_pop

    以下の場合に呼びだされる。parserより。
    * クラス定義終了
    * 特異クラス定義終了
    * モジュール定義終了
    * メソッド定義終了
    * 特異メソッド定義終了
    
    つまり、もっとも深いスコープが消滅するときに呼びだされる
    このスコープにはブロックのスコープは含まれない。
    これは((<Ripper#on__local_push>))と対になっている。
    on__classやon__defを呼びだした直後に呼ばれる

--- Ripper#on__local_push

    以下の場合に呼びだされる。parserより。
    * クラス定義開始
    * 特異クラス定義開始
    * モジュール定義開始
    * メソッド定義開始
    * 特異メソッド定義開始
    
    つまり、新しいスコープが生成されるときに呼びだされる
    このスコープにはブロックのスコープは含まれない。
    これは((<Ripper#on__local_pop>))と対になっている。

--- Ripper#on__massign
--- Ripper#on__mlhs_add
--- Ripper#on__mlhs_add_star
--- Ripper#on__mlhs_aset
--- Ripper#on__mlhs_attrset_colon
--- Ripper#on__mlhs_attrset_dot
--- Ripper#on__mlhs_paren
--- Ripper#on__mlhs_star
--- Ripper#on__mlhs_start
--- Ripper#on__module( module_name, stmts )
    
    モジュール定義時に呼ばれる。parse時に呼びだされる。
    * ((|module_name|)) モジュール名
    * ((|stmts|)) 中の文

--- Ripper#on__name
--- Ripper#on__new_array
--- Ripper#on__new_hash
--- Ripper#on__new_here_document
--- Ripper#on__new_regexp
--- Ripper#on__new_string( str )

    文字列のscanがされる直前に呼びだされる
    * ((|str|)) 文字列開始文字列
      
--- Ripper#on__new_words( str )
    
    %w( で文字列からなる配列を生成する(scanする)直前に呼びだされる
    * ((|str|)) 文字列開始文字列

--- Ripper#on__new_xstring( str )
    
    `で囲まれた文字列や、%x( )でという文字列をscanする直前に呼びだされる
    * ((|str|)) 文字列開始文字列

--- Ripper#on__newline
--- Ripper#on__next
--- Ripper#on__not
--- Ripper#on__notop
--- Ripper#on__opaset
--- Ripper#on__opassign
--- Ripper#on__opcallassign( receiver, method_name, op_assign, arg )
    
    receiver.method_name += arg の形のメソッド呼び出しがされたときに
    呼びだされる。parse時に呼びだされる。
    * ((|receiver|)) レシーバ
    * ((|method_name|)) メソッド
    * ((|op_assign|)) "*="や"+="など
    * ((|arg|)) 右辺

--- Ripper#on__or
--- Ripper#on__paren
--- Ripper#on__redo
--- Ripper#on__regexp_end
--- Ripper#on__regexp_options
--- Ripper#on__rescue
--- Ripper#on__rescue_else
--- Ripper#on__rescue_mod
--- Ripper#on__retry
--- Ripper#on__return
--- Ripper#on__scan
--- Ripper#on__sclass( s_obj, statements )
    
    特異クラス定義時に呼ばれる。parse時に呼びだされる。
    * ((|s_obj|)) 特異クラスを定義するオブジェクトを示すもの
    * ((|statements|)) クラスの中身

--- Ripper#on__sdef( s_obj, method_name, arglist, statements )
    
    特異メソッド定義時に呼ばれる。parse時に呼びだされる。
    * ((|s_obj|)) 特異メソッドを定義するオブジェクトを示すもの
    * ((|method_name|)) メッソド名
    * ((|arglist|)) 仮引数
    * ((|statements|)) メソッドの中身

--- Ripper#on__set_line
--- Ripper#on__space
--- Ripper#on__string_add_dstr
--- Ripper#on__string_concat( str1, str2 )

      スキャナ(lexer)では判定できない文字列連結が生じたときに呼びだされる。
      parser時に呼びだされる
      
--- Ripper#on__string_end
--- Ripper#on__super
--- Ripper#on__symbol
--- Ripper#on__tPOW_ASSIGN
    * bug
--- Ripper#on__toplevel_const_get( const_name )
    
    ::CONST_NAME という方式による定数の参照
    * ((|const_name|)) 定数の名前

--- Ripper#on__unary
--- Ripper#on__undef
--- Ripper#on__unless
--- Ripper#on__unless_mod
--- Ripper#on__until
--- Ripper#on__until_mod
--- Ripper#on__varcall( method_name, arg )
    
    引数なし、かっこなし、かつレシーバなしのメソッド呼びだしで、
    メソッドの名前が「!」や「?」で終わるときに呼びだされる。
    parse時に呼びだされる。
    * ((|method_name|)) メソッド名
    * ((|arg|)) == nil

--- Ripper#on__varref( var_name )
    
    変数を参照したときに呼びだされる。引数なし、かっこなし、かつレシーバなし
    のメソッド呼びだしのときも呼びだされる。parse時に呼びだされる。
    * ((|var_name|)) 変数名

--- Ripper#on__when
--- Ripper#on__while
--- Ripper#on__while_mod
--- Ripper#on__word_space( context, str )

    %w( で、単語間のスペースごとと、最後の単語の後のスペースの所で呼びだされる
    * ((|str|)) 空白文字の列
    
--- Ripper#on__words_end( context, term )
    
    %w( で、スキャン終了時に呼びだされる。
    * ((|term|)) 終端文字

--- Ripper#on__yield


