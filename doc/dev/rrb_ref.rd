= RRB Refarence
RRB�γ�ȯ�ѥ�ե���󥹤Ǥ���

= �⥸�塼��ؿ�
--- RRB.replace_str

= RRB::Script
��ե�������󥰤��оݤȤʤ�ե����뷲�򤢤�魯���饹

== method
--- get_dumped_info
      ��ե�������󥰤��оݤȤʤ륽�����ξ�����ե쥯�����ǽ��
      ���Ѥ���������Τ�((<RRB::DumpedInfo>))�Υ��󥹥��󥹤Ȥ����֤�
      
= RRB::ScriptFile
��ե�������󥰤��оݤȤʤ�ġ��Υե������ɽ�魯���饹

= RRB::DumpedInfo
��ե쥯�����ˤ�ä�����줿�����ɽ�魯��

== included modules
Enumerable

== method
--- classes_having_method( method )
      ((|method|))�Ȥ���̾���Υ᥽�åɤ���ĥ��饹���Ƥ�DumpedClassInfo��
      ����Ȥ����֤���

--- [](index)
      ((|index|))�Ȥ���̾���Υ��饹�ξ����((<RRB::DumpedClassInfo>))��
      ���󥹥��󥹤Ȥ����֤�

      ((|index|))��ʸ���󡢤⤷����((<RRB::Namespace>))�Υ��󥹥��󥹤�
      �ʤ���Фʤ�ʤ��� 
      
--- resolve_const( namespace, const )
      �����̾�����򤹤�

      ((|namespace|))�Ȥ���̾�����־�ˤ���((|const|))�Ȥ��������
      �ºݤˤϤɤΥ͡��ॹ�ڡ�����°���Ƥ����ΤǤ���Τ����֤���
      
      ���ͤ�((<RRB::Namespace>))�Υ��󥹥��󥹤Ǥ��롣
      
= RRB::Replacer
�����Ȥäƥ��������ִ��򤹤롣

== class method
--- new( lineno, pointer, before, after )
      ((|lineno|))���ܡ���Ƭ����((|pointer|))���ܤˤ���((|before|))�Ȥ���
      ʸ�����((|after|))�Ȥ���ʸ������ִ�����Replacer����������
      
--- new_from_id( id, after )
      ((|id|))��Ϳ����줿���̻�( ((<RRB::IdInfo>))�Υ��󥹥��� )��
      ((|after|))�Ȥ���̾�����ִ�����Replacer����������
      
= RRB::DumpedClassInfo
��ե쥯�����ˤ�ä�����줿�ġ��Υ��饹�ξ���

== method
--- type
      "class"���⤷����"module"���֤�
      
--- class_name
      ���饹��̾����ʸ������֤�
      
--- ancestor_names
      �����ѡ����饹��������֤�����ʬ���Ȥϴޤޤʤ���
      
--- public_method_names
      �ѥ֥�å��᥽�åɤ�̾����ʸ�����������֤�
      
--- protected_method_names
      �ץ�ƥ��ƥ��åɥ᥽�åɤ�̾����ʸ�����������֤�
      
--- private_method_names
      �ץ饤�١��ȥ᥽�åɤ�̾����ʸ�����������֤�
      
--- singleton_method_names
      �ðۥ᥽�åɤ�̾����ʸ�����������֤�
      
--- consts
      �����̾����������֤�
      
--- has_method?( methodname, inherited_too=true )
      ���Υ��饹��((|methodname|))�Ȥ���̾���Υ᥽�åɤ���äƤ���ʤ鿿��
      �ʤ���е����֤���
      
      ((|inherited_too|))�����ʤ饹���ѡ����饹�⸫�롣
      
--- subclass_of?(classname)
      self��((|classname|))�Ȥ���̾���Υ��饹�Υ��֥��饹�Ǥ���п���
      �ʤ���е����֤���

      ((|classname|))��ʸ����((<RRB::Namespace>))�Υ��󥹥���
      �Ǥʤ���Фʤ�ʤ�
      
= RRB::NullDumpedClassInfo
((<RRB::DumpedClassInfo>))�Υ��ߡ��Ȥʤ륪�֥������Ȥ��륯�饹
¸�ߤ��ʤ����饹�ξ�������Ȥ���������롣
NullObject�ѥ�����&singleton�ѥ�����򻲾�

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
�ѡ���������������빽ʸ�ڤθġ��ΥΡ��ɤ򤢤�魯��
���Υ��饹�Υ��󥹥��󥹤��������줺��
�ºݤˤϰʲ���RRB::*Node�Ȥ������饹�˷Ѿ����ƻȤ��Ƥ��롣

�ʲ���local_vars�ʤɤǤ�Ʊ�����̻Ҥ�ʣ����Ȥ��Ƥ���Ф��β��ʬ����
����ˤ�����뤳�Ȥ����

== method
--- name_id
      ���ΥΡ��ɤ��б������̾����(MethodNode�ʤ�᥽�å�̾��ClassNode�ʤ�
      ���饹̾)�μ��̻Ҥ��֤�
      
--- class_defs
      ���饹����Ρ��ɤ�����
      
--- method_defs
      �᥽�å�����Ρ��ɤ�������֤�
      
--- method_calls
      ���ΥΡ�����Υ᥽�åɸƤӽФ���((<RRB::IdInfo>))��������֤�
      
--- local_vars
      ���ΥΡ�����Υ������ѿ���((<RRB::IdInfo>))��������֤�
      
--- global_vars
      ���ΥΡ�����Υ����Х��ѿ���((<RRB::IdInfo>))��������֤�
      
--- instance_vars
      ���ΥΡ�����Υ��󥹥����ѿ���((<RRB::IdInfo>))��������֤�
      
--- class_vars
      ���ΥΡ�����Υ��饹�ѿ���((<RRB::IdInfo>))��������֤�
      
--- consts
      ���ΥΡ�����������((<RRB::ConstInfo>))��������֤�
      
--- fcalls
      ���ΥΡ�����Υ쥷���Ф��ά�����᥽�åɸƤӽФ���
      ((<RRB::IdInfo>))��������֤�
      
--- singleton_method_defs
      �ðۥ᥽�å�����Ρ��ɤ��֤�
      
--- class_method_defs
      ���饹�᥽�å�����Ρ��ɤ��֤�
      
--- singleton_class_defs
      �ðۥ��饹����Ρ��ɤ��֤�
      
--- head_keyword
      ��������γ���ͽ���(class,def�ʤ�)��((<RRB::IdInfo>))���֤�
      
--- tail_keyword
      ��������ν�λͽ���(end)��((<RRB::IdInfo>))���֤�
      
--- assigned
      ���ΥΡ�����Υ������ѿ��ǡ�����ʸ�κ��դȤ��ƻȤ��Ƥ����Τ�
      ((<RRB::IdInfo>))��������֤�
      
--- attr_readers
      ���ΥΡ�����Ǥ�attr_reader�ΰ����Ȥʤä�����ܥ���б�����
      ((<RRB::IdInfo>))��������֤�
      
--- attr_writers
      ���ΥΡ�����Ǥ�attr_writer�ΰ����Ȥʤä�����ܥ���б�����
      ((<RRB::IdInfo>))��������֤�

--- attr_accessors
      ���ΥΡ�����Ǥ�attr_accessor�ΰ����Ȥʤä�����ܥ���б�����
      ((<RRB::IdInfo>))��������֤�

--- calls
      fcalls + method_calls���֤�
      
--- name
      �Ρ��ɤΡ�̾���פ�IdInfo/ConstInfo���֤���
      ���饹����ʤ饯�饹̾�򡢥᥽�å�����ʤ�᥽�å�̾��Ȥ����褦�ˡ�

--- range
      ���ΥΡ��ɤ�����ϰ�(head_keyword����tail_keyword�ޤ�)��
      ����魯���֥������Ȥ�((<RRB::SyntaxRange>))��
      ���󥹥��󥹤Ȥ����֤�
      
= RRB::MethodNode
��ʸ�ڤΥ᥽�åɤΥΡ��ɤ�ɽ�魯���饹

== super class
((<RRB::Node>))

= RRB::ToplevelNode
��ʸ�ڤΰ��־�ˤ�����Ρ���

== super class
((<RRB::Node>))

= RRB::ClassMethodNode
��ʸ�ڤΥ��饹�᥽�åɤΥΡ��ɤ�ɽ�魯���饹

== super class
((<RRB::Node>))

= RRB::SingletonClassNode
��ʸ�ڤ��ðۥ��饹����ΥΡ��ɤ�ɽ�魯���饹

== super class
((<RRB::Node>))

= RRB::ClassNode

��ʸ�ڤΥ��饹����ΥΡ��ɤ�ɽ�魯���饹

== super class
((<RRB::ModuleNode>))

== method
--- superclass
      �����С����饹�򼨤�ConstInfo���֤�
      
= RRB::ModuleNode
��ʸ�ڤΥ⥸�塼������ΥΡ��ɤ�ɽ�魯���饹
���ߤ�((<RRB::ClassNode>))�ȤۤȤ�ɶ��̤���Ƥ��ʤ�

== super class
((<RRB::Node>))

= RRB::IdInfo
��������θġ��μ��̻Ҥ�ɽ�魯���饹

== method
--- type
      ���̻Ҥμ�����֤����ʲ��Τ����줫���֤�
      * :id �������ѿ������̤Υ᥽�å�̾�ʤɤμ��̻�
      * :cvar ���饹�ѿ�
      * :fid �����餫�˥᥽�å�̾�Ȥ狼�뼱�̻�(kind_of?,map!�ʤ�)
      * :const ���
      * :ivar ���󥹥����ѿ�
      * :keyword ͽ���
      * :op �黻��
      * :gvar �����Х��ѿ�
      * :symbol ����ܥ�
      * :nil ̵���򤢤�魯
      
--- lineno
      ���μ��̻ҤΤ�����ֹ�
      
--- pointer
      ���μ��̻Ҥ���������ʬ�ι�Ƭ����ΰ���
      
--- name
      ���μ��̻Ҥ�ʸ����
      
--- head_pointer
      ���μ��̻Ҥ�Ƭ����ʬ�ι�Ƭ����ΰ���
= RRB::Replacer
���������ִ���Ĥ򤢤�魯

== class method
--- new( lineno, pointer, before, after )
      ((|lineno|))���ܡ���Ƭ����((|pointer|))���ܤΰ��֤ˤ���((|before|))��
      ����ʸ�����((|after|))�Ȥ���ʸ������֤�������
      
--- new_from_id( id, after )
      ((|id|))�Ȥ������̻Ҥ�((|after|))�ˤ���������
= RRB::ConstInfo
��������������(A::B::C�ʤ�)ɽ�魯���饹
((<RRB::IdInfo>))����Ȥ��ƹͤ�����

== method
--- basename
      �����̾����ʸ����Ǥ�����

      ������((<toplevel?>))�����Ǥ��äƤ⤽��Ƭ��"::"�Ȥ���ʸ�����
      �ޤޤ�ʤ�
      
--- name
      �����̾����ʸ����Ǥ�����

--- toplevel?
      ��������򤢤�魯ʸ���󤬥��������::A::B�Ȥ������Ǥ���п���
      A::B�Ȥ������Ǥ���е����֤�
      
--- elements_id
      ���������������ġ��μ��̻�((<RRB::IdInfo>))��������֤�

--- body
      ��������Ρ����Ρ�(A::B::C�ʤ�C)���б����뼱�̻�((<RRB::IdInfo>))
      ���֤�

= RRB::SyntaxRange
����Ρ��ɤ�����ϰϡ��Ĥޤ��������ͽ���(class�ʤ�)��
�����λͽ���(end)���Ф򤢤�魯��

== method
--- head
      �������ͽ���(class�ʤ�)��((<RRB::IdInfo>))�Υ��󥹥��󥹤��֤�

--- tail
      �����λͽ���(end)��((<RRB::IdInfo>))�Υ��󥹥��󥹤��֤�

--- contain?( range )
      ((|range|))�ǻ��ꤷ���ϰϤ�self���ϰϤ���˴ޤޤ�Ƥ���п���
      �ʤ���е����֤�

--- out_of?( range )
      �����ǻ��ꤷ���ϰϤ�self���ϰϤȸ���ʤ���п��򡢸���е����֤�
      
= RRB::NodeNamespace
Visitor�ǹ�ʸ�ڤ�traverse���Ƥ���Ȥ��ˡ�visit_*��Ϳ�����븽�ߤΥΡ��ɤ�
̾�����֤�ɽ�魯���饹

== class method
--- new( cur_node, cur_ns )
      ((|cur_ns|))�β���((|cur_node|))�򤯤äĤ���̾�����֤�
      ����魯���󥹥��󥹤���������

== method
--- name
      ���Υ��󥹥��󥹤�����魯̾�����֤򤢤�魯ʸ������֤�
      
--- match?( ns )
      ((<RRB::Namespace>))�Ȥΰ���Ƚ��򤹤롣
      ((|ns|))�� RRB::Namespace �Υ��󥹥��󥹤Ǥ���ɬ�פ����롣

--- normal
      self���б�����((<RRB::Namespace>))�Υ��󥹥��󥹤��֤�
      
= RRB::Namespace
̾������(A::B�ʤ�)��ɽ�魯���饹

hash,eql?��������Ƥ���Τ�Set�θ���Hash�Υ����Ȥ������ѤǤ���

== class method
--- new( ns )
--- []( ns )
      ((|ns|))������魯���󥹥��󥹤��������롣

      ������ʸ����ʸ��������Ǥʤ���Фʤ�ʤ���
      
== method
--- name
      ̾�����֤��б�����ʸ������֤�
      
--- ary
      ̾�����֤��б�����������֤�
      
--- ==
      ���������ɤ�����Ƚ�ꤹ��

--- chop
      ������������ʬ���ä���Τ��֤��᥽�å�
      �ĤޤꡢRRB::NS["A::B::C"] ���Ф� RRB::NS["A::B"]���֤�
