= RRB(Ruby Refactoring Browser) ��ȯ���Ϥ˴ؤ��륬����

����ϡ�RRB��ȯ�˻��ä�����ɤΤ褦�ˤ��ƤȤꤢ����ư���ޤǤ�äƤ�����
���������Ƥ��ޤ���

== CVS���饽������ȤäƤ������Ȥν�����ˡ
1. rrb/ripper�ʲ������ꤹ��
   rrb/ripper/README.ja�򻲾Ȥ���
   rrb_ripper.so��Ŭ���ʾ����֤����Ȥ���褦�ˤ���
   ripper.rb �� ��require 'rrb/ripper'�פǥ��ɤǤ�������֤���
   
2. Ruby 1.6�ξ���stringio��pp.rb��pretty_print.rb��shim����ȤäƤ���
   �Ȥ���褦�����ꤹ��

��(����)�ξ�硢rrb/sublib��rrb/sublib/rrb���Ȥ����ǥ��쥯�ȥ���ꡢ
rrb/sublib�� stringio.so, pp.rb, pretty_print.rb, rrb_ripper.so ���֤���
rrb/sublib/rrb �� ripper.rb ���֤��Ƥ��롣

== �ƥ��Ȥ򤹤�
�ƥ��Ȥ�¹Ԥ�����ϥ����ȥǥ��쥯�ȥ��rrb/�Ǥʤ���Фʤ�ʤ���
�ʲ��Τ褦�ˤ��Ƽ¹Ԥ���

  [~/src/rrb]% ruby -Ilib -Isublib tests/all.rb

== �¹ԥե������ư�����Ƥߤ�
�¹ԥե������ bin/rrb �ǡ��ʲ��Τ褦�ˤ��Ƽ¹ԤǤ��ޤ���
  
  [~/src/rrb]% ruby -Ilib -Isublib bin/rrb rename-local-variable 'Rename#method_1' i j < samples/rename_local_variable_stream | lv

  
