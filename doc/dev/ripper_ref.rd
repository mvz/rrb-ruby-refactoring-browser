= Ripper �ʰץ�ե����

== ��
* �����Ǥ���ʸ����Ȥϡ�����Ū��"��'��%q���Ǥ�����줿ʸ�����ҥ��ɥ������
  �ʤɤ�ؤ��������Ǥʤ����ϤǤ��������������դ򤦤ʤ�����

* �֥�å��ȤϤ����륤�ƥ졼���Ǥ���

== ���饹�᥽�å�

--- Ripper.new(*args)
--- Ripper.parse(*args)

== �᥽�å�

--- Ripper#parse(*args)
--- Ripper#pointer

== parse��˸ƤӤ������᥽�å�
  
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
    
    ?a �ʤɤ�ɽ�蘆���������ƥ�뤬scan���줿�Ȥ��˸ƤӤ�����롣
    ��fix
    * ((|str|)) =="?"

--- Ripper#on__CMP
--- Ripper#on__CMP_ASSIGN
--- Ripper#on__COLON
--- Ripper#on__COLON2
--- Ripper#on__COLON3
--- Ripper#on__COMMA
--- Ripper#on__CONSTANT( const_id )
    * ((|const_id|))������̻�
    
--- Ripper#on__CVAR( cvar_id )
    *  ((|cvar_id|)) ���饹�ѿ����̻�

--- Ripper#on__DOLLER
--- Ripper#on__DOT
--- Ripper#on__DOT2
--- Ripper#on__DOT3
--- Ripper#on__END
--- Ripper#on__EQ
--- Ripper#on__EQQ
--- Ripper#on__FID( function_id )

    �������?��!�ʤɤ��դ��Ƥ��롢���餫�˥᥽�åɤȤ狼�뼱�̻�
    * ((|function_id|))���̻�
    
--- Ripper#on__GEQ
--- Ripper#on__GT
--- Ripper#on__GVAR( gvar_id )
    * ((|gvar_id|)) �����Х��ѿ����̻�

--- Ripper#on__HAT
--- Ripper#on__HAT_ASSIGN
--- Ripper#on__IDENTIFIER( id )

    * ((|id|)) ���̻�
    
--- Ripper#on__INTEGER_10
--- Ripper#on__INTEGER_16
--- Ripper#on__INTEGER_2
--- Ripper#on__INTEGER_8
--- Ripper#on__IVAR( ivar_id )
    * ((|ivar_id|)) ���󥹥����ѿ����̻�

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
    
    ?(�����������ޡ���)��scan���줿�Ȥ��˸ƤӤ������
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
    
    ʸ����˼¹Ի���ɾ�������ʸ���� #{...} ��scan���줿�Ȥ��˸ƤӤ�����롣
    * ((|str|)) �ϼ¹Ի���ɾ�������ʸ����
    
--- Ripper#on__add_space( context, str )

    %w( ��ʸ���󤫤�ʤ��������������Ȥ���%w( ��ľ���spaceʸ��������Ȥ�
    ��Ӥ������
    * ((|context|))  %w( �ʤ�
    * ((|str|)) %w( ��ľ���spaceʸ����ʣ��³���Ф��줬�ޤȤ�ƸƤӤ�����롣
      �Ĥޤꡢ%w(  hoge heke) �Ȥ�����Τ��Ф��Ƥ� "  " ���ƤӤ������
      
--- Ripper#on__add_string( context, str )

    ʸ���󤬽������줿�Ȥ��˸ƤӤ�����롣
    ���������ҥ��ɥ�����ȤϤȤꤢ�Ĥ������ۤʤ롣
    �ޤ���"befere #{hoge} after"�Ȥ���ʸ������������ȡ�
    "befere "�� " after"����ʬ���̡��˽�������롣
    �ޤ���#{}����ʬ���̸���(on__add_eval_string�ʤɤ�)���������
    
--- Ripper#on__add_word( context, str )

    %w( ��ʸ���󤫤�ʤ��������������Ȥ�����İ�Ĥ�ʸ���󤴤Ȥ�
    �ƤӤ�����롣
    
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

    ((|var|))�������κ��դȤ��ƻȤ����Ƚ�ꤵ�줿���˸ƤӤ�����롣
    �֥�å��ΰ����Ȥ��ƻȤ�줿�Ȥ���ƤӤ������
    parse���˸ƤӤ�����롣
    * ((|var|)) ����(ʸ���󤽤Τޤ�)
    * ((|arg|)) �᥽�åɤΥǥե���Ȱ����Ȥ�������ʸ���񤫤줿�Ȥ��α��ա�
      ����ʳ��� nil �Ȥʤ�
    
--- Ripper#on__assoc
--- Ripper#on__assoc_add
--- Ripper#on__assoc_list
--- Ripper#on__backref_error
--- Ripper#on__begin
--- Ripper#on__begin_block
--- Ripper#on__begin_brace

    { }  �֥�å����Ϥޤ�Ȥ��˸ƤӤ�����롣parse���˸ƤӤ�����롣
    ((<Ripper#on__end_brace>))���ФˤʤäƤ��롣

--- Ripper#on__begin_do
    
    do end �֥�å����Ϥޤ�Ȥ��˸ƤӤ�����롣parse���˸ƤӤ�����롣
    ((<Ripper#on__end_do>))���ФˤʤäƤ��롣

--- Ripper#on__block
--- Ripper#on__blockcall( method_call, block )

    �֥�å��ե᥽�åɸƤӤ����ΤȤ��˸ƤӤ�����롣

    �����ʤ������ä��ʤ������ĥ쥷���Фʤ��Υ᥽�åɸƤӤ����ǡ��֥�å���
    ������ fcall��varref�ϸƤӤ����줺�ˡ�����Τߤ��ƤӤ������
    ���Ȥ���ա�(ripper.y�������Ƥ��ξ���fcall��Ƥ֤褦�ˤ���)

    * ((|method_call|)) �᥽�å�
    * ((|block|)) �֥�å�

--- Ripper#on__blockvar
--- Ripper#on__break
--- Ripper#on__call( receiver, method, args )
    
    �쥷���Ф���ĥ᥽�åɸƤӽФ����˸ƤӤ�����롣parse���˸ƤӤ�����롣
    * ((|receiver|)) �쥷����
    * ((|method|)) �᥽�å�̾
    * ((|args|)) ����

--- Ripper#on__call_colon
--- Ripper#on__case
--- Ripper#on__class( class_name, stmts, superclass )
    
    ���饹������˸ƤФ�롣parse���˸ƤӤ�����롣
    * ((|class_name|)) ���饹̾
    * ((|stmts|)) ���ʸ
    * ((|superclass|)) �����ѡ����饹��̾�������ꤵ��Ƥ��ʤ����� nil��

--- Ripper#on__comment
--- Ripper#on__comment_start
--- Ripper#on__condexpr
--- Ripper#on__const_get( receiver, const_name )
    
    A::B�Ȥ�������������򻲾Ȥ����Ȥ��˸ƤӤ�����롣parse���˸ƤӤ�����롣
    ::CONST_NAME �Ȥ��������ξ���((<Ripper#on__toplevel_const_get>))��
    �ƤӤ����졢����ϸƤӤ�����ʤ���
    �ޤ�����::�פʤ��Ǥ�������Ȥ�((<Ripper#on__varref>))���ƤӤ����졢
    ����ϸƤӤ�����ʤ���
    * ((|receiver|)) ::�κ�¦
    * ((|const_name|)) ���̾��::�α�¦
    
--- Ripper#on__def( def_str, name, arglist, statements, rescue_clause )
    
    �᥽�å�������˸ƤФ��
    * ((|def_str|)) == "def"
    * ((|name|)) �᥽�å�̾
    * ((|arglist|)) �����Υꥹ��
    * ((|statements|)) �᥽�åɤ����
    * ((|rescue_clause|)) �쥹���塼��
    

--- Ripper#on__defined
--- Ripper#on__else
--- Ripper#on__elsif
--- Ripper#on__embdoc( str )
    
    �����ߥɥ�����Ȥ�1��scan���뤴�Ȥ˸ƤФ��
    * ((|str|)) �����ߥɥ�����Ȥ�1�Ԥ�����(���Դޤ�)

--- Ripper#on__embdoc_begin( str )

    �����ߥɥ�����Ȥ�scan���ϻ��˸ƤФ��
    * ((|str|)) == "=begin"

--- Ripper#on__embdoc_begin_label( str )
    
    �����ߥɥ�����Ȥ�scan���ϻ���on__embdoc_begin�θ�˸ƤӤ������
    * ((|str|)) =begin��³��ʸ����(not Ruby's String)

--- Ripper#on__embdoc_end

    �����ߥɥ�����Ȥ�scan��λ���˸ƤФ��
    * ((|str|)) == "=end"

--- Ripper#on__embdoc_end_label

    �����ߥɥ�����Ȥ�scan��λ����on__embdoc_end�θ�˸ƤӤ������
    * ((|str|)) =end��³��ʸ����(not Ruby's String)

--- Ripper#on__end_brace

    { }  �֥�å��������˸ƤӤ�����롣parse���˸ƤӤ�����롣
    ((<Ripper#on__begin_brace>))���ФˤʤäƤ���

--- Ripper#on__end_do

    do end  �֥�å����Ϥޤ�Ȥ��˸ƤӤ�����롣parse���˸ƤӤ�����롣
    ((<Ripper#on__begin_do>))���ФˤʤäƤ���

--- Ripper#on__ensure
--- Ripper#on__escaped_newline
--- Ripper#on__eval_string_begin( context, str )
    * ((|str|)) == #{
    
--- Ripper#on__eval_string_end( context, str )
    * ((|str|)) == }
    
--- Ripper#on__fcall( function, args )

    �쥷���Ф�̵���᥽�åɸƤӤ����ǸƤФ�롣parse���˸ƤӤ�����롣
    �᥽�åɸƤӤ������������ѿ��ؤΥ������������̤Ǥ��ʤ��Ȥ���
    �ƤӤ�����ʤ���
    * ((|function|)) �ؿ�̾
    * ((|args|)) �����ꥹ��
    
--- Ripper#on__for
--- Ripper#on__here_document_end( context )
    * ((|context|)) �� << �ʤ�ʸ���󳫻�ʸ����
    
--- Ripper#on__here_document_eos( context, eos )
    * ((|context|)) �� << �ʤ�ʸ���󳫻�ʸ����
    * ((|eos|)) ʸ����üʸ����
      
      ����ϰʲ��Υ������ˤ����� "EOS" �ˤ�����
        str == <<EOS
        hoge
         kek
        ped
        EOS
      
--- Ripper#on__here_document_eos_term( context, term )

    <<' �� <<" �ʤɤ�Ȥä��Ȥ��˸ƤӤ������
        
--- Ripper#on__if
--- Ripper#on__if_mod
--- Ripper#on__ignored_newline
--- Ripper#on__infix
--- Ripper#on__is_dyna_regexp
--- Ripper#on__is_once_regexp
--- Ripper#on__is_xstring
    
    ʸ�����scan������ä��塢����ʸ����`�ǰϤޤ줿��ΤǤ��ä����˸ƤӤ���
    ��롣
    �ҥ��ɥ�����Ȥξ���ƤӤ������

--- Ripper#on__lhsadd_value
--- Ripper#on__lhs_aset
    
    ����

--- Ripper#on__lhs_attrset_colon

    ����

--- Ripper#on__lhs_attrset_dot

    ����

--- Ripper#on__list_add
--- Ripper#on__list_start
--- Ripper#on__local_count( arg_name )
    
    �����餯����������ʤ���ǽ���Τ���local�ѿ���Ƚ�ꤹ��
    �����¸�ߤ����ΤȻפ��롣
    ���Τ��ˤ� &block �Ǥ��줬��������Ƥ��ʤ��Τ���ǤϤ��롣
    �������� &block �Ǥ�ƤӤ������褦�ˤ���
    * ((|arg_name|)) ��������̾��
    �ʲ��ξ��˸ƤӤ�����롣parser�����lexer����⡣
    * �᥽�åɤ����̤β�������İ�ĤˤĤ���(parser)
    * �᥽�åɤ����󲽲������ˤĤ���(parser)
    * $~ �� scan ����뤴�Ȥ�(lexer)
    
--- Ripper#on__local_pop

    �ʲ��ξ��˸ƤӤ�����롣parser��ꡣ
    * ���饹�����λ
    * �ðۥ��饹�����λ
    * �⥸�塼�������λ
    * �᥽�å������λ
    * �ðۥ᥽�å������λ
    
    �Ĥޤꡢ��äȤ⿼���������פ����Ǥ���Ȥ��˸ƤӤ������
    ���Υ������פˤϥ֥�å��Υ������פϴޤޤ�ʤ���
    �����((<Ripper#on__local_push>))���ФˤʤäƤ��롣
    on__class��on__def��ƤӤ�����ľ��˸ƤФ��

--- Ripper#on__local_push

    �ʲ��ξ��˸ƤӤ�����롣parser��ꡣ
    * ���饹�������
    * �ðۥ��饹�������
    * �⥸�塼���������
    * �᥽�å��������
    * �ðۥ᥽�å��������
    
    �Ĥޤꡢ�������������פ����������Ȥ��˸ƤӤ������
    ���Υ������פˤϥ֥�å��Υ������פϴޤޤ�ʤ���
    �����((<Ripper#on__local_pop>))���ФˤʤäƤ��롣

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
    
    �⥸�塼��������˸ƤФ�롣parse���˸ƤӤ�����롣
    * ((|module_name|)) �⥸�塼��̾
    * ((|stmts|)) ���ʸ

--- Ripper#on__name
--- Ripper#on__new_array
--- Ripper#on__new_hash
--- Ripper#on__new_here_document
--- Ripper#on__new_regexp
--- Ripper#on__new_string( str )

    ʸ�����scan�������ľ���˸ƤӤ������
    * ((|str|)) ʸ���󳫻�ʸ����
      
--- Ripper#on__new_words( str )
    
    %w( ��ʸ���󤫤�ʤ��������������(scan����)ľ���˸ƤӤ������
    * ((|str|)) ʸ���󳫻�ʸ����

--- Ripper#on__new_xstring( str )
    
    `�ǰϤޤ줿ʸ����䡢%x( )�ǤȤ���ʸ�����scan����ľ���˸ƤӤ������
    * ((|str|)) ʸ���󳫻�ʸ����

--- Ripper#on__newline
--- Ripper#on__next
--- Ripper#on__not
--- Ripper#on__notop
--- Ripper#on__opaset
--- Ripper#on__opassign
--- Ripper#on__opcallassign( receiver, method_name, op_assign, arg )
    
    receiver.method_name += arg �η��Υ᥽�åɸƤӽФ������줿�Ȥ���
    �ƤӤ�����롣parse���˸ƤӤ�����롣
    * ((|receiver|)) �쥷����
    * ((|method_name|)) �᥽�å�
    * ((|op_assign|)) "*="��"+="�ʤ�
    * ((|arg|)) ����

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
    
    �ðۥ��饹������˸ƤФ�롣parse���˸ƤӤ�����롣
    * ((|s_obj|)) �ðۥ��饹��������륪�֥������Ȥ򼨤����
    * ((|statements|)) ���饹�����

--- Ripper#on__sdef( s_obj, method_name, arglist, statements )
    
    �ðۥ᥽�å�������˸ƤФ�롣parse���˸ƤӤ�����롣
    * ((|s_obj|)) �ðۥ᥽�åɤ�������륪�֥������Ȥ򼨤����
    * ((|method_name|)) ��å���̾
    * ((|arglist|)) ������
    * ((|statements|)) �᥽�åɤ����

--- Ripper#on__set_line
--- Ripper#on__space
--- Ripper#on__string_add_dstr
--- Ripper#on__string_concat( str1, str2 )

      �������(lexer)�Ǥ�Ƚ��Ǥ��ʤ�ʸ����Ϣ�뤬�������Ȥ��˸ƤӤ�����롣
      parser���˸ƤӤ������
      
--- Ripper#on__string_end
--- Ripper#on__super
--- Ripper#on__symbol
--- Ripper#on__tPOW_ASSIGN
    * bug
--- Ripper#on__toplevel_const_get( const_name )
    
    ::CONST_NAME �Ȥ��������ˤ������λ���
    * ((|const_name|)) �����̾��

--- Ripper#on__unary
--- Ripper#on__undef
--- Ripper#on__unless
--- Ripper#on__unless_mod
--- Ripper#on__until
--- Ripper#on__until_mod
--- Ripper#on__varcall( method_name, arg )
    
    �����ʤ������ä��ʤ������ĥ쥷���Фʤ��Υ᥽�åɸƤӤ����ǡ�
    �᥽�åɤ�̾������!�פ��?�פǽ����Ȥ��˸ƤӤ�����롣
    parse���˸ƤӤ�����롣
    * ((|method_name|)) �᥽�å�̾
    * ((|arg|)) == nil

--- Ripper#on__varref( var_name )
    
    �ѿ��򻲾Ȥ����Ȥ��˸ƤӤ�����롣�����ʤ������ä��ʤ������ĥ쥷���Фʤ�
    �Υ᥽�åɸƤӤ����ΤȤ���ƤӤ�����롣parse���˸ƤӤ�����롣
    * ((|var_name|)) �ѿ�̾

--- Ripper#on__when
--- Ripper#on__while
--- Ripper#on__while_mod
--- Ripper#on__word_space( context, str )

    %w( �ǡ�ñ��֤Υ��ڡ������Ȥȡ��Ǹ��ñ��θ�Υ��ڡ����ν�ǸƤӤ������
    * ((|str|)) ����ʸ������
    
--- Ripper#on__words_end( context, term )
    
    %w( �ǡ��������λ���˸ƤӤ�����롣
    * ((|term|)) ��üʸ��

--- Ripper#on__yield


