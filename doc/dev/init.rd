= RRB(Ruby Refactoring Browser) ��ȯ���Ϥ˴ؤ��륬����

����ϡ�RRB��ȯ�˻��ä�����ɤΤ褦�ˤ��ƤȤꤢ����ư���ޤǤ�äƤ�����
���������Ƥ��ޤ���

== CVS���饽������ȤäƤ������Ȥν�����ˡ
1. rrb/ripper�ʲ������ꤹ��
   rrb/ripper/README.ja�򻲾Ȥ���
   rrb_ripper.so��Ŭ���ʾ����֤����Ȥ���褦�ˤ���
   ripper.rb �� ��require 'rrb/ripper'�פǥ��ɤǤ�������֤���
   
2. rrb/reflection�ʲ������ꤹ��
   rrb/reflection �� cd ���ơ�`ruby extconf.rb; make' ��¹Ԥ��롣
   
3. Ruby 1.6�ξ���shim�򥤥󥹥ȡ��뤹�롣
   shim�Ȥ�1.8�ε�ǽ��1.6�Ǥ������ټ¸����뤿��Υ饤�֥��Ǥ��ꡢ
   �ܤ�����((<URL:http://raa.ruby-lang.org/list.rhtml?name=shim-ruby16_18>))��
   ���Ȥ��Ƥ���������
   
��(����)�ξ�硢rrb/sublib��rrb/sublib/rrb���Ȥ����ǥ��쥯�ȥ���ꡢ
rrb/sublib�� rrb_ripper.so ���֤���rrb/sublib/rrb �� ripper.rb ���֤��Ƥ��롣

== �ƥ��Ȥ򤹤�
�ƥ��Ȥ�¹Ԥ�����ϥ����ȥǥ��쥯�ȥ��rrb/�Ǥʤ���Фʤ�ʤ���
�ʲ��Τ褦�ˤ��Ƽ¹Ԥ���

  [~/src/rrb]% RUBYLIB=$RUBYLIB:~/src/rrb/reflection ruby -Ilib -Isublib tests/all.rb

== �¹ԥե������ư�����Ƥߤ�
�¹ԥե������ bin/rrb �ǡ��ʲ��Τ褦�ˤ��Ƽ¹ԤǤ��ޤ���
  
  [~/src/rrb]% RUBYLIB=$RUBYLIB:~/src/rrb/reflection ruby -Ilib -Isublib bin/rrb --rename-local-variable 'Rename#method_1' i j < samples/rename_var_sample_stream > output

  
