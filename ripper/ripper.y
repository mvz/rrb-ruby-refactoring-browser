/* vi:set sw=4: */
/*

    ripper.y

    derived from ruby/parse.y
    original header is:

        Author: matz
        Date: 2001/05/16 09:05:50
        created at: Fri May 28 18:02:42 JST 1993

        Copyright (C) 1993-2001 Yukihiro Matsumoto

    modified by Minero Aoki since 2001

*/

%{

#define RIPPER_VERSION  "0.0.5"

#define YYDEBUG 1
#include "ruby.h"
#include "env.h"
#include "node.h"
#include "st.h"
#include <stdio.h>
#include <errno.h>
#include <ctype.h>

#define ID_SCOPE_SHIFT 3
#define ID_SCOPE_MASK 0x07
#define ID_LOCAL    0x01
#define ID_INSTANCE 0x02
#define ID_GLOBAL   0x03
#define ID_ATTRSET  0x04
#define ID_CONST    0x05
#define ID_CLASS    0x06
#define ID_JUNK     0x07
#define ID_INTERNAL ID_JUNK

#define is_notop_id(id) ((id)>LAST_TOKEN)
#define is_local_id(id) (is_notop_id(id)&&((id)&ID_SCOPE_MASK)==ID_LOCAL)
#define is_global_id(id) (is_notop_id(id)&&((id)&ID_SCOPE_MASK)==ID_GLOBAL)
#define is_instance_id(id) (is_notop_id(id)&&((id)&ID_SCOPE_MASK)==ID_INSTANCE)
#define is_attrset_id(id) (is_notop_id(id)&&((id)&ID_SCOPE_MASK)==ID_ATTRSET)
#define is_const_id(id) (is_notop_id(id)&&((id)&ID_SCOPE_MASK)==ID_CONST)
#define is_class_id(id) (is_notop_id(id)&&((id)&ID_SCOPE_MASK)==ID_CLASS)


#ifndef RIPPER
NODE *ruby_eval_tree_begin = 0;
NODE *ruby_eval_tree = 0;
#endif

#ifndef RIPPER
char *ruby_sourcefile;		/* current source file */
int   ruby_sourceline;		/* current line no. */
#else
#  define ruby_sourcefile   (parser->source_file)
#  define ruby_sourceline   (parser->source_line)
#endif

enum lex_state_e {
    EXPR_BEG,			/* ignore newline, +/- is a sign. */
    EXPR_END,			/* newline significant, +/- is a operator. */
    EXPR_ARG,			/* newline significant, +/- is a operator. */
    EXPR_CMDARG,                /* newline significant, +/- is a operator. */
    EXPR_ENDARG,                /* newline significant, +/- is a operator. */
    EXPR_MID,			/* newline significant, +/- is a operator. */
    EXPR_FNAME,			/* ignore newline, no reserved words. */
    EXPR_DOT,			/* right after `.' or `::', no reserved words. */
    EXPR_CLASS,			/* immediate after `class', no here document. */
};
#ifndef RIPPER
static enum lex_state_e lex_state;
#else
#  define lex_state         (parser->state)
#endif

#ifdef HAVE_LONG_LONG
typedef unsigned LONG_LONG stack_type;
#else
typedef unsigned long stack_type;
#endif

#ifndef RIPPER
static stack_type cond_stack = 0;
#else
#  define cond_stack        (parser->condition_stack)
#endif
#define COND_PUSH(n) do {                      \
    cond_stack = (cond_stack<<1)|((n)&1);      \
} while(0)
#define COND_POP() do {                        \
    cond_stack >>= 1;                          \
} while (0)
#define COND_LEXPOP() do {                     \
    int last = COND_P();                       \
    cond_stack >>= 1;                          \
    if (last) cond_stack |= 1;                 \
} while (0)
#define COND_P() (cond_stack&1)

#ifndef RIPPER
static stack_type cmdarg_stack = 0;
#else
#  define cmdarg_stack      (parser->commandarg_stack)
#endif
#define CMDARG_PUSH(n) do {                    \
    cmdarg_stack = (cmdarg_stack<<1)|((n)&1);  \
} while(0)
#define CMDARG_POP() do {                      \
    cmdarg_stack >>= 1;                        \
} while (0)
#define CMDARG_LEXPOP() do {                   \
    int last = CMDARG_P();                     \
    cmdarg_stack >>= 1;                        \
    if (last) cmdarg_stack |= 1;               \
} while (0)
#define CMDARG_P() (cmdarg_stack&1)

#ifndef RIPPER
static int class_nest = 0;
static int in_single = 0;
static int in_def = 0;
static int compile_for_eval = 0;
static ID cur_mid = 0;
#else
#  define class_nest        (parser->class_nesting)
#  define in_single         (parser->in_singleton)
#  define in_def            (parser->in_method)
#  define compile_for_eval  (parser->compiling_for_eval)
#  define cur_mid           (parser->current_method_id)
#endif

#ifndef RIPPER
static int in_defined = 0;
#else
#  define in_defined        (parser->in_defined_arg)
#endif


#define dispatch0(name)           rb_funcall(parser->value, rip_id_ ## name, 0)
#define dispatch1(name,a)         rb_funcall(parser->value, rip_id_ ## name, 1,a)
#define dispatch2(name,a,b)       rb_funcall(parser->value, rip_id_ ## name, 2,a,b)
#define dispatch3(name,a,b,c)     rb_funcall(parser->value, rip_id_ ## name, 3,a,b,c)
#define dispatch4(name,a,b,c,d)   rb_funcall(parser->value, rip_id_ ## name, 4,a,b,c,d)
#define dispatch5(name,a,b,c,d,e) rb_funcall(parser->value, rip_id_ ## name, 5,a,b,c,d,e)

#define s_dispatch1(name,a)      (dispatch1(scan,a), dispatch1(name,a))
#define s_dispatch2(name,a,b)    (dispatch1(scan,b), dispatch2(name,a,b))

#include "dispids.h"

/* --------------- ripper class struct ------------------- */

struct ruby_parser {
    VALUE value;
    VALUE result;

/* parser */

    int parsing;

    int source_line;
    VALUE source_file;

    stack_type condition_stack;
    stack_type commandarg_stack;
    int rip_command_start;

    int heredocument_end;
    int __end__seen;

    int compiling_for_eval;   /* dummy */

    int class_nesting;
    int in_method;
    int in_singleton;
    int in_defined_arg;

    VALUE current_method_id;
    VALUE last_token_id;

/* scanner */

    enum lex_state_e state;

    char *token_buffer;
    int token_index;
    int token_size;
    VALUE src;
    VALUE last_line;
    char *ptr_beg;
    char *ptr;
    char *ptr_end;
    VALUE (*gets)();
    int gets_ptr;

/* lval */

    void *tmp_yylval;
};


/* --------------------- yacc related -------------------- */

#define YYPARSE_PARAM parser_v
#define YYLEX_PARAM   parser_v

#define parser ((struct ruby_parser *)parser_v)

#undef yyparse
#undef yylex
#undef yyerror
#define yyparse      rip_yyparse
#define yylex(a,b)   rip_yylex(a,b)
#define yyerror(s)   rip_yyerror(parser, s)

#undef yylval
#undef yydebug
#define yydebug rip_yydebug

static int rip_yylex();
static int rip_yyerror _((struct ruby_parser *, char *));


/* --------------------- function prototypes -------------------- */

#ifdef RIPPER
static VALUE rip_tok2sym _((char*));
static VALUE rip_id2sym _((ID));
#endif

#ifndef RIPPER
static NODE *cond();
static NODE *logop();
#else
#  define cond(n)  n
#endif

#ifndef RIPPER
static NODE *newline_node();
static void fixpos();
#else
#  define fixpos(a,b)
#endif

#ifndef RIPPER
static int value_expr();
static void void_expr();
static void void_stmts();
#else
#  define value_expr(n)
#  define void_expr(n)
#  define void_stmts(n)
#endif

#ifndef RIPPER
static NODE *block_append();
static NODE *list_append();
static NODE *list_concat();
static NODE *arg_concat();
static NODE *arg_prepend();
static NODE *call_op();
#endif

#ifndef RIPPER
static NODE *ret_args();
static NODE *arg_blk_pass();
static NODE *new_call();
static NODE *new_fcall();
static NODE *new_super();
#endif

#ifndef RIPPER
static NODE *gettable();
static NODE *assignable();
static NODE *aryset();
static NODE *attrset();
static void rb_backref_error();
static NODE *node_assign();
#else
#  define assignable(l,r)  dispatch2(assignable,l,r)
#  define rb_backref_error(s)  dispatch1(backref_error, s)
#endif

#ifndef RIPPER
static NODE *match_gen();
static void local_push();
static void local_pop();
static int  local_append();
static int  local_cnt();
static int  local_id();
static ID  *local_tbl();
static ID   internal_id();
#else
#  define local_cnt(node) dispatch1(local_count, node)
#  define local_push()    dispatch0(local_push)
#  define local_pop()     dispatch0(local_pop)
#  define local_id(n)     0
#endif

#ifndef RIPPER
static struct RVarmap *dyna_push();
static void dyna_pop();
static int dyna_in_block();
#endif

#ifndef RIPPER
static void top_local_init();
static void top_local_setup();
#endif

#ifdef RIPPER
#  define lex_get_str(str)        rip_lex_get_str(parser,str)
#  define lex_getline()           rip_lex_getline(parser)
#  define nextc()                 rip_nextc(parser)
#  define pushback(c)             rip_pushback(parser,c)
#  define newtok()                rip_newtok(parser)
#  define tokadd(c)               rip_tokadd(parser,c)
#  define read_escape()           rip_read_escape(parser)
#  define tokadd_escape(t)        rip_tokadd_escape(parser,t)
#  define parse_regx(c,t,p)       rip_parse_regx(parser,c,t,p)
#  define parse_string(c,f,t,p)   rip_parse_string(parser,c,f,t,p)
#  define parse_qstring(c,t,p)    rip_parse_qstring(parser,c,t,p)
#  define parse_quotedwords(c,t,p)  rip_parse_quotedwords(parser,c,t,p)
#  define parse_here_document(t,i)  rip_parse_here_document(parser,t,i)
static int rip_parse_regx        _((struct ruby_parser*,VALUE,int,int));
static int rip_parse_string      _((struct ruby_parser*,VALUE,int,int,int));
static int rip_parse_qstring     _((struct ruby_parser*,VALUE,int,int));
static int rip_parse_quotedwords _((struct ruby_parser*,VALUE,int,int));
static int rip_parse_here_document _((struct ruby_parser*,int,int));
#endif

%}

%pure_parser

%union {
    NODE *node;
    VALUE val;
    ID id;
    int num;
    struct RVarmap *vars;
}

%token<val>  kCLASS
	kMODULE
	kDEF
	kUNDEF
	kBEGIN
	kRESCUE
	kENSURE
	kEND
	kIF
	kUNLESS
	kTHEN
	kELSIF
	kELSE
	kCASE
	kWHEN
	kWHILE
	kUNTIL
	kFOR
	kBREAK
	kNEXT
	kREDO
	kRETRY
	kIN
	kDO
	kDO_COND
	kDO_BLOCK
	kRETURN
	kYIELD
	kSUPER
	kSELF
	kNIL
	kTRUE
	kFALSE
	kAND
	kOR
	kNOT
	kIF_MOD
	kUNLESS_MOD
	kWHILE_MOD
	kUNTIL_MOD
	kRESCUE_MOD
	kALIAS
	kDEFINED
	klBEGIN
	klEND
	k__LINE__
	k__FILE__

%token <val> tIDENTIFIER tFID tGVAR tIVAR tCONSTANT tCVAR
%token <val> tINTEGER tFLOAT tSTRING tXSTRING tREGEXP
%token <val> tDSTRING tDXSTRING tDREGEXP tNTH_REF tBACK_REF

%type <val> singleton string
%type <val>  literal numeric
%type <val> compstmt stmts stmt expr arg primary command command_call method_call
%type <val> if_tail opt_else case_body cases rescue exc_list exc_var ensure
%type <val> args when_args call_args call_args2 open_args paren_args opt_paren_args
%type <val> command_args aref_args opt_block_arg block_arg var_ref
%type <val> mrhs mrhs_basic superclass block_call block_command
%type <val> f_arglist f_args f_optarg f_opt f_block_arg opt_f_block_arg
%type <val> assoc_list assocs assoc undef_list backref
%type <val> block_var opt_block_var brace_block do_block lhs none
%type <val> mlhs mlhs_head mlhs_basic mlhs_entry mlhs_item mlhs_node
%type <val>   fitem variable sym symbol operation operation2 operation3
%type <val>   cname fname op f_rest_arg
%type <num>  f_norm_arg f_arg
%type <val> reswords then term do terms opt_terms opt_nl trailer  /* ripper */
%token tUPLUS 		/* unary+ */
%token tUMINUS 		/* unary- */
%token tPOW		/* ** */
%token tCMP  		/* <=> */
%token tEQ  		/* == */
%token tEQQ  		/* === */
%token tNEQ  		/* != */
%token tGEQ  		/* >= */
%token tLEQ  		/* <= */
%token tANDOP tOROP	/* && and || */
%token tMATCH tNMATCH	/* =~ and !~ */
%token tDOT2 tDOT3	/* .. and ... */
%token tAREF tASET	/* [] and []= */
%token tLSHFT tRSHFT	/* << and >> */
%token tCOLON2		/* :: */
%token tCOLON3		/* :: at EXPR_BEG */
%token <val> tOP_ASGN   /* +=, -=  etc. */
%token tASSOC		/* => */
%token tLPAREN		/* ( */
%token tLPAREN_ARG      /* ( */
%token tRPAREN		/* ) */
%token tLBRACK		/* [ */
%token tLBRACE		/* { */
%token tLBRACE_ARG      /* { */
%token tSTAR		/* * */
%token tAMPER		/* & */
%token tSYMBEG

/*
 *	precedence table
 */

%left  kIF_MOD kUNLESS_MOD kWHILE_MOD kUNTIL_MOD kRESCUE_MOD
%left  kOR kAND
%right kNOT
%nonassoc kDEFINED
%right '=' tOP_ASGN
%right '?' ':'
%nonassoc tDOT2 tDOT3
%left  tOROP
%left  tANDOP
%nonassoc  tCMP tEQ tEQQ tNEQ tMATCH tNMATCH
%left  '>' tGEQ '<' tLEQ
%left  '|' '^'
%left  '&'
%left  tLSHFT tRSHFT
%left  '+' '-'
%left  '*' '/' '%'
%right '!' '~' tUPLUS tUMINUS
%right tPOW

%token LAST_TOKEN

%%
program		:  {
                        VALUE tmp;

			lex_state = EXPR_BEG;
                        tmp = rb_ivar_get(parser->value,
                                          rb_intern("@_topclass"));
			if (tmp == rb_cObject) class_nest = 0;
			else class_nest = 1;
		    }
		  compstmt
                    {
                        parser->result = $2;
                    }

compstmt	: stmts opt_terms
		    {
			$$ = $1;
		    }

stmts		: none
		| stmt
		| stmts terms stmt
		| error stmt
		    {
			$$ = $2;
		    }

stmt		: kALIAS fitem {lex_state = EXPR_FNAME;} fitem
		    {
			if (in_def || in_single)
			    yyerror("alias within method");
                        $$ = dispatch2(alias, $2, $4);
		    }
		| kALIAS tGVAR tGVAR
		    {
			if (in_def || in_single)
			    yyerror("alias within method");
		        $$ = dispatch2(alias, $2, $3);
		    }
		| kALIAS tGVAR tBACK_REF
		    {
			if (in_def || in_single)
			    yyerror("alias within method");
		        $$ = dispatch2(alias, $2, $3);
		    }
		| kALIAS tGVAR tNTH_REF
		    {
		        yyerror("can't make alias for the number variables");
		        $$ = Qnil;
		    }
		| kUNDEF undef_list
		    {
			if (in_def || in_single)
			    yyerror("undef within method");
			$$ = dispatch1(undef, $2);
		    }
		| stmt kIF_MOD expr
		    {
                        $$ = dispatch2(if_mod, $1, $3);
		    }
		| stmt kUNLESS_MOD expr
		    {
                        $$ = dispatch2(unless_mod, $1, $3);
		    }
		| stmt kWHILE_MOD expr
		    {
                        $$ = dispatch2(while_mod, $1, $3);
		    }
		| stmt kUNTIL_MOD expr
		    {
                        $$ = dispatch2(until_mod, $1, $3);
		    }
		| stmt kRESCUE_MOD stmt
		    {
			$$ = dispatch2(rescue_mod, $1, $3);
		    }
		| klBEGIN
		    {
			if (in_def || in_single) {
			    yyerror("BEGIN in method");
			}
		    }
		  '{' compstmt '}'
		    {
                        $$ = dispatch1(BEGIN, $4);
		    }
		| klEND '{' compstmt '}'
		    {
			if (compile_for_eval && (in_def || in_single)) {
			    yyerror("END in method; use at_exit");
			}

                        $$ = dispatch1(END, $3);
		    }
		| lhs '=' command_call
		    {
                        $$ = dispatch2(assign, $1, $3);
		    }
		| mlhs '=' command_call
		    {
                        $3 = dispatch2(argadd_value, Qnil, $3);
                        $$ = dispatch2(massign, $1, $3);
		    }
		| lhs '=' mrhs_basic
		    {
                        $1 = dispatch2(lhsadd_value, Qnil, $1);
                        $$ = dispatch2(massign, $1, $3);
		    }
		| mlhs '=' mrhs
		    {
                        $$ = dispatch2(massign, $1, $3);
		    }
		| expr

expr		: kRETURN call_args
		    {
			if (!compile_for_eval && !in_def && !in_single)
			    yyerror("return appeared outside of method");
                        $$ = dispatch1(return, $2);
		    }
                | kBREAK call_args
                    {
                        $$ = dispatch1(break, $2);
                    }
                | kNEXT call_args
                    {
                        $$ = dispatch1(next, $2);
                    }
		| command_call
		| expr kAND expr
		    {
                        $$ = dispatch2(and, $1, $3);
		    }
		| expr kOR expr
		    {
                        $$ = dispatch2(or, $1, $3);
		    }
		| kNOT expr
		    {
                        $$ = dispatch1(not, $2);
		    }
		| '!' command_call
		    {
                        $$ = dispatch1(notop, $2);
		    }
		| arg

command_call	: command
		| block_command

block_command	: block_call
		| block_call '.' operation2 command_args
		    {
                        $$ = dispatch3(call, $1, $3, $4);
		    }
		| block_call tCOLON2 operation2 command_args
		    {
                        $$ = dispatch3(call, $1, $3, $4);
		    }

command		: operation command_args
		    {
                        $$ = dispatch2(fcall, $1, $2);
		   }
		| primary '.' operation2 command_args
		    {
                        $$ = dispatch3(call, $1, $3, $4);
		    }
		| primary tCOLON2 operation2 command_args
		    {
                        $$ = dispatch3(call, $1, $3, $4);
		    }
		| kSUPER command_args
		    {
			if (!compile_for_eval && !in_def && !in_single)
			    yyerror("super called outside of method");
                        $$ = dispatch1(super, $2);
		    }
		| kYIELD call_args
		    {
                        $$ = dispatch1(yield, $2);
		    }

mlhs		: mlhs_basic
		| tLPAREN mlhs_entry ')'
		    {
			$$ = $2;
		    }

mlhs_entry	: mlhs_basic
		| tLPAREN mlhs_entry ')'
		    {
			$$ = $2;
		    }

mlhs_basic	: mlhs_head
		| mlhs_head mlhs_item
		    {
                        $$ = dispatch2(mlhs_add, $1, $2);
		    }
		| mlhs_head tSTAR mlhs_node
		    {
                        $$ = dispatch2(mlhs_add_star, $1, $3);
		    }
		| mlhs_head tSTAR
		    {
                        $$ = dispatch1(mlhs_star, $1);
		    }
		| tSTAR mlhs_node
		    {
                        $$ = dispatch1(mlhs_add_star, $2);
		    }
		| tSTAR
		    {
                        $$ = dispatch0(mlhs_star);
		    }

mlhs_item	: mlhs_node
		| tLPAREN mlhs_entry ')'
		    {
                        $$ = dispatch1(mlhs_paren, $2);
                        /* $$ = $2; */
		    }

mlhs_head	: mlhs_item ','
		    {
                        $$ = dispatch1(mlhs_start, $1);
		    }
		| mlhs_head mlhs_item ','
		    {
                        $$ = dispatch2(mlhs_add, $1, $2);
		    }

mlhs_node	: variable
                    {
                        $$ = assignable($1, Qnil);
                    }
		| primary '[' aref_args ']'
		    {
                        $$ = dispatch2(mlhs_aset, $1, $3);
		    }
		| primary '.' tIDENTIFIER
		    {
                        $$ = dispatch2(mlhs_attrset_dot, $1, $3);
		    }
		| primary tCOLON2 tIDENTIFIER
		    {
                        $$ = dispatch2(mlhs_attrset_colon, $1, $3);
		    }
		| primary '.' tCONSTANT
		    {
                        $$ = dispatch2(mlhs_attrset_dot, $1, $3);
		    }
		| backref
		    {
		        $$ = rb_backref_error($1);
		    }

lhs		: variable
                    {
                        $$ = assignable($1, Qnil);
                    }
		| primary '[' aref_args ']'
		    {
                        $$ = dispatch2(lhs_aset, $1, $3);
		    }
		| primary '.' tIDENTIFIER
		    {
                        $$ = dispatch2(lhs_attrset_dot, $1, $3);
		    }
		| primary tCOLON2 tIDENTIFIER
		    {
                        $$ = dispatch2(lhs_attrset_colon, $1, $3);
		    }
		| primary '.' tCONSTANT
		    {
                        $$ = dispatch2(lhs_attrset_dot, $1, $3);
		    }
		| backref
		    {
		        $$ = rb_backref_error($1);
		    }

cname		: tIDENTIFIER
		    {
			yyerror("class/module name must be CONSTANT");
		    }
		| tCONSTANT

fname		: tIDENTIFIER
		| tCONSTANT
		| tFID
		| op
		    {
			lex_state = EXPR_END;
			$$ = $1;
		    }
		| reswords
		    {
			lex_state = EXPR_END;
			$$ = $<id>1;
		    }

fitem		: fname
		| symbol

undef_list	: fitem
		    {
                        $$ = dispatch1(list_start, $1);
		    }
		| undef_list ',' {lex_state = EXPR_FNAME;} fitem
		    {
                        $$ = dispatch2(list_add, $1, $4);
		    }

op		: '|'		{ $$ = ID2SYM('|'); }
		| '^'		{ $$ = ID2SYM('^'); }
		| '&'		{ $$ = ID2SYM('&'); }
		| tCMP		{ $$ = rip_tok2sym("<=>"); }
		| tEQ		{ $$ = rip_tok2sym("=="); }
		| tEQQ		{ $$ = rip_tok2sym("==="); }
		| tMATCH	{ $$ = rip_tok2sym("=~"); }
		| '>'		{ $$ = ID2SYM('>'); }
		| tGEQ		{ $$ = rip_tok2sym(">="); }
		| '<'		{ $$ = ID2SYM('<'); }
		| tLEQ		{ $$ = rip_tok2sym("<="); }
		| tLSHFT	{ $$ = rip_tok2sym("<<"); }
		| tRSHFT	{ $$ = rip_tok2sym(">>"); }
		| '+'		{ $$ = ID2SYM('+'); }
		| '-'		{ $$ = ID2SYM('-'); }
		| '*'		{ $$ = ID2SYM('*'); }
		| tSTAR		{ $$ = ID2SYM('*'); }
		| '/'		{ $$ = ID2SYM('/'); }
		| '%'		{ $$ = ID2SYM('%'); }
		| tPOW		{ $$ = rip_tok2sym("**"); }
		| '~'		{ $$ = ID2SYM('~'); }
		| tUPLUS	{ $$ = rip_tok2sym("+@"); }
		| tUMINUS	{ $$ = rip_tok2sym("-@"); }
		| tAREF		{ $$ = rip_tok2sym("[]"); }
		| tASET		{ $$ = rip_tok2sym("[]="); }
		| '`'		{ $$ = ID2SYM('`'); }

reswords	: k__LINE__ | k__FILE__  | klBEGIN | klEND
		| kALIAS | kAND | kBEGIN | kBREAK | kCASE | kCLASS | kDEF
		| kDEFINED | kDO | kELSE | kELSIF | kEND | kENSURE | kFALSE
		| kFOR | kIF_MOD | kIN | kMODULE | kNEXT | kNIL | kNOT
		| kOR | kREDO | kRESCUE | kRETRY | kRETURN | kSELF | kSUPER
		| kTHEN | kTRUE | kUNDEF | kUNLESS_MOD | kUNTIL_MOD | kWHEN
		| kWHILE_MOD | kYIELD | kRESCUE_MOD

arg		: lhs '=' arg
		    {
                        $$ = dispatch2(assign, $1, $3);
		    }
		| variable tOP_ASGN {$$ = assignable($1, Qnil);} arg
		    {
                        $$ = dispatch3(opassign, $1, $2, $4)
		    }
		| primary '[' aref_args ']' tOP_ASGN arg
		    {
                        $$ = dispatch4(opaset, $1, $3, $5, $6);
		    }
		| primary '.' tIDENTIFIER tOP_ASGN arg
		    {
                        $$ = dispatch4(opcallassign, $1, $3, $4, $5);
		    }
		| primary '.' tCONSTANT tOP_ASGN arg
		    {
                        $$ = dispatch4(opcallassign, $1, $3, $4, $5);
		    }
		| primary tCOLON2 tIDENTIFIER tOP_ASGN arg
		    {
                        $$ = dispatch4(opcallassign, $1, $3, $4, $5);
		    }
		| backref tOP_ASGN arg
		    {
		        $$ = rb_backref_error($1);
		    }
		| arg tDOT2 arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym(".."), $3);
		    }
		| arg tDOT3 arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("..."), $3);
		    }
		| arg '+' arg
		    {
                        $$ = dispatch3(infix, $1, ID2SYM('+'), $3);
		    }
		| arg '-' arg
		    {
                        $$ = dispatch3(infix, $1, ID2SYM('-'), $3);
		    }
		| arg '*' arg
		    {
                        $$ = dispatch3(infix, $1, ID2SYM('*'), $3);
		    }
		| arg '/' arg
		    {
                        $$ = dispatch3(infix, $1, ID2SYM('/'), $3);
		    }
		| arg '%' arg
		    {
                        $$ = dispatch3(infix, $1, ID2SYM('%'), $3);
		    }
		| arg tPOW arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("**"), $3);
		    }
		| tUPLUS arg
		    {
                        $$ = dispatch2(unary, rip_tok2sym("+@"), $2);
		    }
		| tUMINUS arg
		    {
                        $$ = dispatch2(unary, rip_tok2sym("-@"), $2);
		    }
		| arg '|' arg
		    {
                        $$ = dispatch3(infix, $1, ID2SYM('|'), $3);
		    }
		| arg '^' arg
		    {
                        $$ = dispatch3(infix, $1, ID2SYM('^'), $3);
		    }
		| arg '&' arg
		    {
                        $$ = dispatch3(infix, $1, ID2SYM('&'), $3);
		    }
		| arg tCMP arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("<=>"), $3);
		    }
		| arg '>' arg
		    {
                        $$ = dispatch3(infix, $1, ID2SYM('>'), $3);
		    }
		| arg tGEQ arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym(">="), $3);
		    }
		| arg '<' arg
		    {
                        $$ = dispatch3(infix, $1, ID2SYM('<'), $3);
		    }
		| arg tLEQ arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("<="), $3);
		    }
		| arg tEQ arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("=="), $3);
		    }
		| arg tEQQ arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("==="), $3);
		    }
		| arg tNEQ arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("!="), $3);
		    }
		| arg tMATCH arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("=~"), $3);
		    }
		| arg tNMATCH arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("!~"), $3);
		    }
		| '!' arg
		    {
                        $$ = dispatch2(unary, ID2SYM('!'), $2);
		    }
		| '~' arg
		    {
                        $$ = dispatch2(unary, ID2SYM('~'), $2);
		    }
		| arg tLSHFT arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("<<"), $3);
		    }
		| arg tRSHFT arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym(">>"), $3);
		    }
		| arg tANDOP arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("&&"), $3);
		    }
		| arg tOROP arg
		    {
                        $$ = dispatch3(infix, $1, rip_tok2sym("||"), $3);
		    }
		| kDEFINED opt_nl {in_defined = 1;} arg
		    {
		        in_defined = 0;
                        $$ = dispatch1(defined, $4);
		    }
		| arg '?' arg ':' arg
		    {
                        $$ = dispatch3(condexpr, $1, $3, $5);
		    }
		| primary

aref_args	: none
		| command opt_nl
		    {
                        $$ = $1;
		    }
		| args trailer
		    {
                        $$ = $1;
		    }
		| args ',' tSTAR arg opt_nl
		    {
			value_expr($4);
                        $$ = dispatch2(argadd, $1, $4);
		    }
		| assocs trailer
		    {
			$$ = $1;
		    }
		| tSTAR arg opt_nl
		    {
                        $$ = dispatch0(argvoid);
                        $$ = dispatch1(argadd_star, $2);
			value_expr($2);
		    }

paren_args	: '(' none ')'
		    {
                        $$ = Qnil
		    }
		| '(' call_args opt_nl ')'
		    {
                        $$ = $2;
		    }
		| '(' block_call opt_nl ')'
		    {
                        $$ = $2;
		    }
		| '(' args ',' block_call opt_nl ')'
		    {
                        $$ = dispatch2(argadd, $2, $4);
		    }

opt_paren_args	: none
		| paren_args

call_args	: command
                    {
                        $$ = dispatch2(argadd_value, Qnil, $1);
                    }
		| args opt_block_arg
                    {
                        if (!NIL_P($2)) $$ = dispatch2(argadd_block, $1, $2);
                    }
		| args ',' tSTAR arg opt_block_arg
		    {
			value_expr($4);
                        $$ = dispatch2(argadd_star, $1, $4);
                        if (!NIL_P($5)) $$ = dispatch2(argadd_block, $$, $5);
		    }
		| assocs opt_block_arg
		    {
                        $$ = dispatch2(argadd_assocs, Qnil, $1);
                        if (!NIL_P($2)) $$ = dispatch2(argadd_block, $$, $2);
		    }
		| assocs ',' tSTAR arg opt_block_arg
		    {
			value_expr($4);
                        $$ = dispatch2(argadd_assocs, Qnil, $1);
                        $$ = dispatch2(argadd_star, $$, $4);
                        if (!NIL_P($5)) $$ = dispatch2(argadd_block, $$, $5);
		    }
		| args ',' assocs opt_block_arg
		    {
                        $$ = dispatch2(argadd_assocs, $1, $3);
                        if (!NIL_P($4)) $$ = dispatch2(argadd_block, $$, $4);
		    }
		| args ',' assocs ',' tSTAR arg opt_block_arg
		    {
			value_expr($6);
                        $$ = dispatch2(argadd_assocs, $1, $3);
                        $$ = dispatch2(argadd_star, $$, $6);
                        if (!NIL_P($7)) $$ = dispatch2(argadd_block, $$, $7);
		    }
		| tSTAR arg opt_block_arg
		    {
			value_expr($2);
                        $$ = dispatch2(argadd_star, Qnil, $2);
                        if (!NIL_P($3)) $$ = dispatch2(argadd_block, $$, $3);
		    }
		| block_arg
                    {
                        $$ = dispatch2(argadd_block, Qnil, $1);
                    }

call_args2      : arg ',' args opt_block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd, $$, $1);
                        $$ = dispatch2(argadd_args, $$, $3);
                        $$ = dispatch2(argadd_block, $$, $4);
                    }
                | arg ',' tSTAR arg opt_block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd, $$, $1);
                        $$ = dispatch2(argadd_star, $$, $4);
                        $$ = dispatch2(argadd_block, $$, $5);
                    }
                | arg ',' args ',' tSTAR arg opt_block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd, $$, $1);
                        $$ = dispatch2(argadd_args, $$, $3);
                        $$ = dispatch2(argadd_star, $$, $6);
                        $$ = dispatch2(argadd_block, $$, $7);
                    }
                | assocs opt_block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd_assocs, $$, $1);
                        $$ = dispatch2(argadd_block, $$, $2);
                    }
                | assocs ',' tSTAR arg opt_block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd_assocs, $$, $1);
                        $$ = dispatch2(argadd_star, $$, $4);
                        $$ = dispatch2(argadd_block, $$, $5);
                    }
                | arg ',' assocs opt_block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd, $$, $1);
                        $$ = dispatch2(argadd_assocs, $$, $3);
                        $$ = dispatch2(argadd_block, $$, $4);
                    }
                | arg ',' args ',' assocs opt_block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd, $$, $1);
                        $$ = dispatch2(argadd_args, $$, $3);
                        $$ = dispatch2(argadd_assocs, $$, $5);
                        $$ = dispatch2(argadd_block, $$, $6);
                    }
                | arg ',' assocs ',' tSTAR arg opt_block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd, $$, $1);
                        $$ = dispatch2(argadd_assocs, $$, $3);
                        $$ = dispatch2(argadd_star, $$, $6);
                        $$ = dispatch2(argadd_block, $$, $7);
                    }
                | arg ',' args ',' assocs ',' tSTAR arg opt_block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd, $$, $1);
                        $$ = dispatch2(argadd_args, $$, $3);
                        $$ = dispatch2(argadd_assocs, $$, $5);
                        $$ = dispatch2(argadd_star, $$, $8);
                        $$ = dispatch2(argadd_block, $$, $9);
                    }
                | tSTAR arg opt_block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd_star, $$, $2);
                        $$ = dispatch2(argadd_block, $$, $3);
                    }
                | block_arg
                    {
                        $$ = dispatch0(argstart);
                        $$ = dispatch2(argadd_block, $$, $1);
                    }

command_args    :   {
                        $<num>$ = cmdarg_stack;
                        CMDARG_PUSH(1);
                    }
                  open_args
                    {
                        /* CMDARG_POP() */
                        cmdarg_stack = $<num>1;
                        $$ = $2;
                    }

open_args       : call_args
                | tLPAREN_ARG {lex_state = EXPR_ENDARG;} ')'
                    {
                        rb_warning("%s (...) interpreted as method call",
                                   rb_id2name(SYM2ID($<val>1)));
                        $$ = Qnil;
                    }
                | tLPAREN_ARG call_args2 {lex_state = EXPR_ENDARG;} ')'
                    {
                        CMDARG_POP();
                        rb_warning("%s (...) interpreted as method call",
                                   rb_id2name(SYM2ID($<val>1)));
                        $$ = $2;
                    }

block_arg	: tAMPER arg
		    {
			value_expr($2);
			$$ = $2;
		    }

opt_block_arg	: ',' block_arg
		    {
			$$ = $2;
		    }
		| none

args 		: arg
		    {
			value_expr($1);
                        $$ = dispatch1(argstart, $1);
		    }
		| args ',' arg
		    {
			value_expr($3);
                        $$ = dispatch2(argadd_value, $1, $3);
		    }

mrhs		: arg
		    {
			value_expr($1);
                        $$ = dispatch2(argadd_value, Qnil, $1);
		    }
		| mrhs_basic

mrhs_basic	: args ',' arg
		    {
			value_expr($3);
                        $$ = dispatch2(argadd_value, $1, $3);
		    }
		| args ',' tSTAR arg
		    {
			value_expr($4);
                        $$ = dispatch2(argadd_star, $1, $4);
		    }
		| tSTAR arg
		    {
			value_expr($2);
                        $$ = dispatch2(argadd_star, Qnil, $2);
		    }

primary		: literal
		| string
		| tXSTRING
                        /*
		    {
			$$ = NEW_XSTR($1);
		    }
                        */
		| tDXSTRING
		| tDREGEXP
		| var_ref
		| backref
		| tFID
		    {
                        $$ = dispatch2(varcall, $1, Qnil);
		    }
		| kBEGIN
		  compstmt
		  rescue
		  opt_else
		  ensure
		  kEND
		    {
                        $$ = dispatch4(begin_block, $2, $3, $4, $5);
                    /*
                        if (NIL_P($3) && NIL_P($4) && NIL_P($5)) {
                            $$ = dispatch1(block, $2);
                        }
			else {
                            if (NIL_P($3) && !NIL_P($4))
				rb_warn("else without rescue is useless");
                            $$ = dispatch4(begin, $2, $3, $4, $5);
			}
		        fixpos($$, $2);
                    */
		    }
                | tLPAREN_ARG expr {lex_state = EXPR_ENDARG;} ')'
                    {
                        rb_warning("%s (...) interpreted as grouped expression",
                                   rb_id2name(SYM2ID($<val>1)));
                        $$ = $2;
                    }
		| tLPAREN compstmt ')'
		    {
			$$ = dispatch1(paren, $2);
		    }
		| primary tCOLON2 tCONSTANT
		    {
			value_expr($1);
                        $$ = dispatch2(const_get, $1, $3);
		    }
		| tCOLON3 cname
		    {
                        $$ = dispatch1(toplevel_const_get, $2);
		    }
		| primary '[' aref_args ']'
		    {
			value_expr($1);
			$$ = dispatch2(aref, $1, $3);
		    }
		| tLBRACK aref_args ']'
		    {
                        $$ = dispatch1(new_array, $2);
		    }
		| tLBRACE assoc_list '}'
		    {
			$$ = dispatch1(new_hash, $2);
		    }
		| kRETURN
		    {
			if (!compile_for_eval && !in_def && !in_single)
			    yyerror("return appeared outside of method");
                        $$ = dispatch0(argstart);
			$$ = dispatch1(return, $$);
		    }
		| kYIELD '(' call_args ')'
		    {
			value_expr($3);
			$$ = dispatch1(yield, $3);
		    }
		| kYIELD '(' ')'
		    {
                        $$ = dispatch0(argstart);
			$$ = dispatch1(yield, $$);
		    }
		| kYIELD
		    {
			$$ = dispatch1(yield, Qnil);
		    }
		| kDEFINED opt_nl '(' {in_defined = 1;} expr ')'
		    {
		        in_defined = 0;
			$$ = dispatch1(defined, $5);
		    }
		| operation brace_block
		    {
		        $1 = dispatch2(fcall, $1, Qnil);
                        $$ = dispatch2(blockcall, $1, $2);
		    }
		| method_call
		| method_call brace_block
		    {
                        /*
			if ($1 && nd_type($1) == NODE_BLOCK_PASS) {
			    rb_compile_error("both block arg and actual block given");
			}
                        */
                        $$ = dispatch2(blockcall, $1, $2);
		        fixpos($$, $1);
		    }
		| kIF expr then
		  compstmt
		  if_tail
		  kEND
		    {
			value_expr($2);
			$$ = dispatch3(if, cond($2), $4, $5);
		        fixpos($$, $2);
		    }
		| kUNLESS expr then
		  compstmt
		  opt_else
		  kEND
		    {
			value_expr($2);
			$$ = dispatch3(unless, cond($2), $4, $5);
		        fixpos($$, $2);
		    }
		| kWHILE {COND_PUSH(1);} expr do {COND_POP();}
		  compstmt
		  kEND
		    {
			value_expr($3);
			$$ = dispatch2(while, cond($3), $6);
		        fixpos($$, $3);
		    }
		| kUNTIL {COND_PUSH(1);} expr do {COND_POP();} 
		  compstmt
		  kEND
		    {
			value_expr($3);
			$$ = dispatch2(until, cond($3), $6);
		        fixpos($$, $3);
		    }
		| kCASE expr opt_terms
		  case_body
		  kEND
		    {
			value_expr($2);
			$$ = dispatch2(case, $2, $4);
		        fixpos($$, $2);
		    }
		| kCASE opt_terms case_body kEND
		    {
			$$ = dispatch2(case, Qnil, $3);
		    }
		| kFOR block_var kIN {COND_PUSH(1);} expr do {COND_POP();}
		  compstmt
		  kEND
		    {
			value_expr($5);
			$$ = dispatch3(for, $2, $5, $8);
		        fixpos($$, $2);
		    }
		| kCLASS cname superclass
		    {
			if (in_def || in_single)
			    yyerror("class definition in method body");
			class_nest++;
			local_push();
		        $<num>$ = ruby_sourceline;
		    }
		  compstmt
		  kEND
		    {
		        $$ = dispatch3(class, $2, $5, $3);
		        /* nd_set_line($$, $<num>4); */
                        dispatch2(set_line, $$, INT2NUM($<num>4));
		        local_pop();
			class_nest--;
		    }
		| kCLASS tLSHFT expr
		    {
			$<num>$ = in_def;
		        in_def = 0;
		    }
		  term
		    {
		        $<num>$ = in_single;
		        in_single = 0;
			class_nest++;
			local_push();
		    }
		  compstmt
		  kEND
		    {
		        $$ = dispatch2(sclass, $3, $7);
		        fixpos($$, $3);
		        local_pop();
			class_nest--;
		        in_def = $<num>4;
		        in_single = $<num>6;
		    }
		| kMODULE cname
		    {
			if (in_def || in_single)
			    yyerror("module definition in method body");
			class_nest++;
			local_push();
		        $<num>$ = ruby_sourceline;
		    }
		  compstmt
		  kEND
		    {
		        $$ = dispatch2(module, $2, $4);
		        /* nd_set_line($$, $<num>3); */
                        dispatch1(set_line, INT2NUM($<num>3));
		        local_pop();
			class_nest--;
		    }
		| kDEF fname
		    {
			if (in_def || in_single)
			    yyerror("nested method definition");
			$<id>$ = cur_mid;
			cur_mid = $2;
			in_def++;
			local_push();
		    }
		  f_arglist
		  compstmt
		  rescue
		  opt_else
		  ensure
		  kEND
		    {
                        $$ = dispatch2(rescue, $5, $6);
                        $$ = dispatch2(rescue_else, $$, $7);
                        $$ = dispatch2(ensure, $$, $8);
                    /*
		        if (!NIL_P($6)) $5 = dispatch3(rescue, $5, $6, $7);
			else if (!NIL_P($7)) {
			    rb_warn("else without rescue is useless");
			    $5 = block_append($5, $7);
			}
			if ($8) $5 = NEW_ENSURE($5, $8);
                    */

                        $$ = dispatch5(def, $1, $2, $4, $$, $6);
                    /*
		        / * NOEX_PRIVATE for toplevel * /
			$$ = NEW_DEFN($2, $4, $5, class_nest?NOEX_PUBLIC:NOEX_PRIVATE);
			if (is_attrset_id($2)) $$->nd_noex = NOEX_PUBLIC;
                    */
		        fixpos($$, $4);
		        local_pop();
			in_def--;
			cur_mid = $<id>3;
		    }
		| kDEF singleton dot_or_colon {lex_state = EXPR_FNAME;} fname
		    {
			value_expr($2);
			in_single++;
			local_push();
		        lex_state = EXPR_END; /* force for args */
		    }
		  f_arglist
		  compstmt
		  rescue
		  opt_else
		  ensure
		  kEND
		    {
                        if (!NIL_P($9) || !NIL_P($10) || !NIL_P($11)) {
                            $8 = dispatch4(rescue, $8, $9, $10, $11);
                        }
                    /*
		        if ($9) $8 = NEW_RESCUE($8, $9, $10);
			else if ($10) {
			    rb_warn("else without rescue is useless");
			    $8 = block_append($8, $10);
			}
			if ($11) $8 = NEW_ENSURE($8, $11);
                    */

			$$ = dispatch4(sdef, $2, $5, $7, $8);
		        fixpos($$, $2);
		        local_pop();
			in_single--;
		    }
		| kBREAK
		    {
			$$ = dispatch1(break, Qnil);
		    }
		| kNEXT
		    {
			$$ = dispatch1(next, Qnil);
		    }
		| kREDO
		    {
			$$ = dispatch0(redo);
		    }
		| kRETRY
		    {
			$$ = dispatch0(retry);
		    }

then		: term
		| kTHEN
		| term kTHEN

do		: term
		| kDO_COND

if_tail		: opt_else
		| kELSIF expr then
		  compstmt
		  if_tail
		    {
			value_expr($2);
			$$ = dispatch3(elsif, cond($2), $4, $5);
		        fixpos($$, $2);
		    }

opt_else	: none
		| kELSE compstmt
		    {
			$$ = dispatch1(else, $2);
		    }

block_var	: lhs
		| mlhs

opt_block_var	: none
		| '|' /* none */ '|'
		    {
			$$ = dispatch1(blockvar, Qnil);
		    }
		| tOROP
		    {
			$$ = dispatch1(blockvar, Qnil);
		    }
		| '|' block_var '|'
		    {
			$$ = dispatch1(blockvar, $2);
		    }

do_block	: kDO_BLOCK
		    {
                        dispatch0(begin_do);
		    }
		  opt_block_var
		  compstmt
		  kEND
		    {
			$$ = dispatch2(block, $3, $4);
		        fixpos($$, $3?$3:$4);
                        dispatch0(end_do);
		    }
                | tLBRACE_ARG {$<val>$ = dispatch0(begin_brace);}
                  opt_block_var
                  compstmt
                  '}'
                    {
                        $$ = dispatch2(block, $3, $4);
		        fixpos($$, $3?$3:$4);
                        dispatch0(end_brace);
                    }

block_call	: command do_block
		    {
                    /*
			if ($1 && nd_type($1) == NODE_BLOCK_PASS) {
			    rb_compile_error("both block arg and actual block given");
			}
			$2->nd_iter = $1;
			$$ = $2;
                    */
                        $$ = dispatch2(blockcall, $1, $2);
		        fixpos($$, $2);
		    }
		| block_call '.' operation2 opt_paren_args
		    {
			value_expr($1);
			$$ = dispatch3(call, $1, $3, $4);
		    }
		| block_call tCOLON2 operation2 opt_paren_args
		    {
			value_expr($1);
			$$ = dispatch3(call, $1, $3, $4);
		    }

method_call	: operation paren_args
		    {
			$$ = dispatch2(fcall, $1, $2);
		        fixpos($$, $2);
		    }
		| primary '.' operation2 opt_paren_args
		    {
			value_expr($1);
			$$ = dispatch3(call, $1, $3, $4);
		        fixpos($$, $1);
		    }
		| primary tCOLON2 operation2 paren_args
		    {
			value_expr($1);
			$$ = dispatch3(call, $1, $3, $4);
		        fixpos($$, $1);
		    }
		| primary tCOLON2 operation3
		    {
                        dispatch3(call, $1, $3, Qnil);
		    }
		| kSUPER paren_args
		    {
			if (!compile_for_eval && !in_def &&
		            !in_single && !in_defined)
			    yyerror("super called outside of method");
                        $$ = dispatch1(super, $2);
		    }
		| kSUPER
		    {
			if (!compile_for_eval && !in_def &&
		            !in_single && !in_defined)
			    yyerror("super called outside of method");
                        $$ = dispatch1(super, Qnil);
		    }

brace_block	: '{'
		    {
                        dispatch0(begin_brace);
		    }
		  opt_block_var
		  compstmt '}'
		    {
			$$ = dispatch2(block, $3, $4);
		        fixpos($$, $4);
                        dispatch0(end_brace);
		    }
		| kDO
		    {
                        dispatch0(begin_do);
		    }
		  opt_block_var
		  compstmt kEND
		    {
			$$ = dispatch2(block, $3, $4);
		        fixpos($$, $4);
                        dispatch0(end_do);
		    }

case_body	: kWHEN when_args then
		  compstmt
		  cases
		    {
			$$ = dispatch3(when, $2, $4, $5);
		    }

when_args	: args
		| args ',' tSTAR arg
		    {
			value_expr($4);
                        $$ = dispatch2(argadd_star, $1, $4);
		    }
		| tSTAR arg
		    {
			value_expr($2);
                        $$ = dispatch1(argstart, $2);
		    }

cases		: opt_else
		| case_body

exc_list	: none
		| args

exc_var		: tASSOC lhs
		    {
			$$ = $2;
		    }
		| none

rescue		: kRESCUE exc_list exc_var then
		  compstmt
		  rescue
		    {
                        /*
		        if ($3) {
		            $3 = node_assign($3, NEW_GVAR(rb_intern("$!")));
			    $5 = block_append($3, $5);
			}
			$$ = NEW_RESBODY($2, $5, $6);
                        */
                        $$ = dispatch4(rescue, $2, $3, $5, $6);
		        fixpos($$, $2?$2:$5);
		    }
		| none

ensure		: none
		| kENSURE compstmt
		    {
                        $$ = dispatch1(ensure, $2);
                        /*
			if ($2)
			    $$ = $2;
			else
			    / * place holder * /
			    $$ = NEW_NIL();
                        */
		    }

literal		: numeric
		| symbol
		| tREGEXP

string		: tSTRING
		    {
                        /*
			$$ = NEW_STR($1);
                        */
                        $$ = $1;
		    }
		| tDSTRING
		| string tSTRING
		    {
                        $$ = dispatch2(string_concat, $1, $2);
                    /*
		        if (nd_type($1) == NODE_DSTR) {
			    list_append($1, NEW_STR($2));
			}
			else {
			    rb_str_concat($1->nd_lit, $2);
			}
			$$ = $1;
                    */
		    }
		| string tDSTRING
		    {
                        $$ = dispatch2(string_add_dstr, $1, $2);
                    /*
		        if (nd_type($1) == NODE_STR) {
			    $$ = NEW_DSTR($1->nd_lit);
			}
			else {
			    $$ = $1;
			}
			$2->nd_head = NEW_STR($2->nd_lit);
			nd_set_type($2, NODE_ARRAY);
			list_concat($$, $2);
                    */
		    }

symbol		: tSYMBEG sym
		    {
		        lex_state = EXPR_END;
                        $$ = dispatch1(symbol, $2);
		    }

sym		: fname
		| tIVAR
		| tGVAR
		| tCVAR

numeric		: tINTEGER
		| tFLOAT

variable	: tIDENTIFIER
		| tIVAR
		| tGVAR
		| tCONSTANT
		| tCVAR
		| kNIL {$$ = rip_tok2sym("nil");}
		| kSELF {$$ = rip_tok2sym("self");}
		| kTRUE {$$ = rip_tok2sym("true");}
		| kFALSE {$$ = rip_tok2sym("false");}
		| k__FILE__ {$$ = rip_tok2sym("__FILE__");}
		| k__LINE__ {$$ = rip_tok2sym("__LINE__");}

var_ref		: variable
		    {
                        $$ = dispatch1(varref, $1);
                    /*
			$$ = gettable($1);
                    */
		    }

backref		: tNTH_REF
		| tBACK_REF

superclass	: term
		    {
			$$ = Qnil;
		    }
		| '<'
		    {
			lex_state = EXPR_BEG;
		    }
		  expr term
		    {
			$$ = $3;
		    }
		| error term {yyerrok; $$ = Qnil;}

f_arglist	: '(' f_args opt_nl ')'
		    {
			lex_state = EXPR_BEG;
                        $$ = $2;
		    }
		| f_args term
		    {
                        $$ = $1;
		    }

f_args		: f_arg ',' f_optarg ',' f_rest_arg opt_f_block_arg
		    {
                        $$ = $1;
                        $$ = dispatch2(argadd_opt, $$, $3);
                        $$ = dispatch2(argadd_rest, $$, $5);
                        $$ = dispatch2(argadd_block, $$, $6);
		    }
		| f_arg ',' f_optarg opt_f_block_arg
		    {
                        $$ = $1;
                        $$ = dispatch2(argadd_opt, $$, $3);
                        $$ = dispatch2(argadd_block, $$, $4);
		    }
		| f_arg ',' f_rest_arg opt_f_block_arg
		    {
                        $$ = $1;
                        $$ = dispatch2(argadd_rest, $$, $3);
                        $$ = dispatch2(argadd_block, $$, $4);
		    }
		| f_arg opt_f_block_arg
		    {
                        $$ = $1;
                        $$ = dispatch2(argadd_block, $$, $2);
		    }
		| f_optarg ',' f_rest_arg opt_f_block_arg
		    {
                        $$ = dispatch0(argvoid);
                        $$ = dispatch2(argadd_opt, $$, $1);
                        $$ = dispatch2(argadd_rest, $$, $3);
                        $$ = dispatch2(argadd_block, $$, $4);
		    }
		| f_optarg opt_f_block_arg
		    {
                        $$ = dispatch0(argvoid);
                        $$ = dispatch2(argadd_opt, $$, $1);
                        $$ = dispatch2(argadd_block, $$, $2);
		    }
		| f_rest_arg opt_f_block_arg
		    {
                        $$ = dispatch0(argvoid);
                        $$ = dispatch2(argadd_rest, $$, $1);
                        $$ = dispatch2(argadd_opt, $$, $2);
		    }
		| f_block_arg
		    {
                        $$ = dispatch0(argvoid);
                        $$ = dispatch2(argadd_block, $$, $1);
		    }
		| /* none */
		    {
                        $$ = dispatch0(argvoid);
		    }

f_norm_arg	: tCONSTANT
		    {
			yyerror("formal argument cannot be a constant");
		    }
                | tIVAR
		    {
                        yyerror("formal argument cannot be an instance variable");
		    }
                | tGVAR
		    {
                        yyerror("formal argument cannot be a global variable");
		    }
                | tCVAR
		    {
                        yyerror("formal argument cannot be a class variable");
		    }
		| tIDENTIFIER
		    {
#ifndef RRB_RIPPER
			if (!is_local_id(SYM2ID($1)))
			    yyerror("formal argument must be local variable");
			else if (local_id($1))
			    yyerror("duplicate argument name");
#endif
			local_cnt($1);
			$$ = $1;
		    }

f_arg		: f_norm_arg
                    {
                        $$ = dispatch1(argstart, $1);
                    }
		| f_arg ',' f_norm_arg
		    {
                        $$ = dispatch2(argadd, $1, $3);
		    }

f_opt		: tIDENTIFIER '=' arg
		    {
#ifndef RRB_RIPPER
			if (!is_local_id(SYM2ID($1)))
			    yyerror("formal argument must be local variable");
			else if (local_id($1))
			    yyerror("duplicate optional argument name");
#endif
			$$ = assignable($1, $3);
		    }

f_optarg	: f_opt
		    {
                        $$ = dispatch1(argstart, $1);
		    }
		| f_optarg ',' f_opt
		    {
                        $$ = dispatch2(argadd, $1, $3);
		    }

f_rest_arg	: tSTAR tIDENTIFIER
		    {
#ifndef RRB_RIPPER
			if (!is_local_id(SYM2ID($2)))
			    yyerror("rest argument must be local variable");
			else if (local_id($2))
			    yyerror("duplicate rest argument name");
#endif
			$$ = local_cnt($2);
		    }
		| tSTAR
		    {
			$$ = Qnil;
		    }

f_block_arg	: tAMPER tIDENTIFIER
		    {
#ifndef RRB_RIPPER
			if (!is_local_id(SYM2ID($2)))
			    yyerror("block argument must be local variable");
			else if (local_id($2))
			    yyerror("duplicate block argument name");
#endif
			$$ = local_cnt($2);
		    }

opt_f_block_arg	: ',' f_block_arg
		    {
			$$ = $2;
		    }
		| none

singleton	: var_ref
                        /*
		    {
			if (nd_type($1) == NODE_SELF) {
			    $$ = NEW_SELF();
			}
			else {
			    $$ = $1;
			}
		    }
                        */
		| '(' {lex_state = EXPR_BEG;} expr opt_nl ')'
		    {
			$$ = $3;
                    /*
			switch (nd_type($3)) {
			  case NODE_STR:
			  case NODE_DSTR:
			  case NODE_XSTR:
			  case NODE_DXSTR:
			  case NODE_DREGX:
			  case NODE_LIT:
			  case NODE_ARRAY:
			  case NODE_ZARRAY:
			    yyerror("can't define single method for literals.");
			  default:
			    break;
			}
			$$ = $3;
                    */
		    }

assoc_list	: none
                    {
                        $$ = dispatch1(assoc_list, Qnil);
                    }
		| assocs trailer
		    {
			$$ = $1;
		    }
		| args trailer
		    {
                        $$ = dispatch1(assoc_list, $1);
                    /*
			if ($1->nd_alen%2 != 0) {
			    yyerror("odd number list for Hash");
			}
			$$ = $1;
                    */
		    }

assocs		: assoc
                    {
                        $$ = dispatch2(assoc_add, Qnil, $1);
                    }
		| assocs ',' assoc
		    {
                        $$ = dispatch2(assoc_add, $1, $3);
		    }

assoc		: arg tASSOC arg
		    {
                        $$ = dispatch2(assoc, $1, $3);
		    }

operation	: tIDENTIFIER
		| tCONSTANT
		| tFID

operation2	: tIDENTIFIER
		| tCONSTANT
		| tFID
		| op

operation3	: tIDENTIFIER
		| tFID
		| op

dot_or_colon	: '.'
		| tCOLON2

opt_terms	: none
		| terms

opt_nl		: none
		| '\n'
                    {
                        $$ = Qnil;
                    }

trailer		: /* none */
                    {
                        $$ = Qnil;
                    }
		| '\n'
                    {
                        $$ = Qnil;
                    }
		| ','
                    {
                        $$ = Qnil;
                    }

term		: ';' {yyerrok; $$ = Qnil;}
		| '\n'
                    {
                        $$ = Qnil;
                    }

terms		: term
		| terms ';' {yyerrok;}

none		: /* none */
		    {
			$$ = Qnil;
		    }
%%


#undef parser   /* reset */
#define yylval       (*((YYSTYPE*)(parser->tmp_yylval)))


#include "regex.h"
#include "util.h"

/* We remove any previous definition of `SIGN_EXTEND_CHAR',
   since ours (we hope) works properly with all combinations of
   machines, compilers, `char' and `unsigned char' argument types.
   (Per Bothner suggested the basic approach.)  */
#undef SIGN_EXTEND_CHAR
#if __STDC__
# define SIGN_EXTEND_CHAR(c) ((signed char)(c))
#else  /* not __STDC__ */
/* As in Harbison and Steele.  */
# define SIGN_EXTEND_CHAR(c) ((((unsigned char)(c)) ^ 128) - 128)
#endif
#define is_identchar(c) (SIGN_EXTEND_CHAR(c)!=-1&&(ISALNUM(c) || (c) == '_' || ismbchar(c)))

#ifndef RIPPER
static char *tokenbuf = NULL;
static int   tokidx, toksiz = 0;
#else
#  define tokenbuf          (parser->token_buffer)
#  define tokidx            (parser->token_index)
#  define toksiz            (parser->token_size)
#endif

#ifndef RIPPER
static NODE *str_extend();
#else
static int rip_str_extend _((struct ruby_parser*, VALUE, char));
#define str_extend(l,t)         rip_str_extend(parser,l,t)
#endif

#define LEAVE_BS 1

#ifndef RIPPER
static VALUE (*lex_gets)();	/* gets function */
static VALUE lex_input;		/* non-nil if File */
static VALUE lex_lastline;	/* gc protect */
static char *lex_pbeg;
static char *lex_p;
static char *lex_pend;
#else
#  define lex_input         (parser->src)
#  define lex_lastline      (parser->last_line)
#  define lex_pbeg          (parser->ptr_beg)
#  define lex_p             (parser->ptr)
#  define lex_pend          (parser->ptr_end)
#  define lex_gets          (parser->gets)
#endif

#ifdef RIPPER
static int
rip_yyerror(parser, msg)
    struct ruby_parser *parser;
    char *msg;
{
    rb_raise(rb_eArgError, "\nRipper::%s:%d: %s",
             RSTRING(parser->source_file)->ptr,
             parser->source_line,
             msg);
    return 0;
}
#else
static int
yyerror(msg)
    char *msg;
{
    char *p, *pe, *buf;
    int len, i;

    rb_compile_error("%s", msg);
    p = lex_p;
    while (lex_pbeg <= p) {
	if (*p == '\n') break;
	p--;
    }
    p++;

    pe = lex_p;
    while (pe < lex_pend) {
	if (*pe == '\n') break;
	pe++;
    }

    len = pe - p;
    if (len > 4) {
	buf = ALLOCA_N(char, len+2);
	MEMCPY(buf, p, char, len);
	buf[len] = '\0';
	rb_compile_error_append("%s", buf);

	i = lex_p - p;
	p = buf; pe = p + len;

	while (p < pe) {
	    if (*p != '\t') *p = ' ';
	    p++;
	}
	buf[i] = '^';
	buf[i+1] = '\0';
	rb_compile_error_append("%s", buf);
    }

    return 0;
}
#endif

#ifndef RIPPER
static int heredoc_end;
static int command_start = Qtrue;

int ruby_in_compile = 0;
int ruby__end__seen;

static VALUE ruby_debug_lines;
#else
#  define heredoc_end       (parser->heredocument_end)
#  define command_start     (parser->rip_command_start)

#  define ruby_in_compile   (parser->parsing)
#  define ruby__end__seen   (parser->__end__seen)
/* we do not use debug_lines */
#endif

#ifndef RIPPER
static int lex_gets_ptr;
#else
#  define lex_gets_ptr      (parser->gets_ptr)
#endif

static VALUE
rip_get_line_String(parser)
    struct ruby_parser *parser;
{
    VALUE s = parser->src;
    char *beg, *end, *pend;

    beg = RSTRING(s)->ptr;
    if (lex_gets_ptr) {
	if (RSTRING(s)->len == lex_gets_ptr) return Qnil;
	beg += lex_gets_ptr;
    }
    pend = RSTRING(s)->ptr + RSTRING(s)->len;
    end = beg;
    while (end < pend) {
	if (*end++ == '\n') break;
    }
    lex_gets_ptr = end - RSTRING(s)->ptr;
    return rb_str_new(beg, end - beg);
}

static VALUE
rip_get_line_File(parser)
    struct ruby_parser *parser;
{
    return rb_io_gets(parser->src);
}

static ID rip_id_gets;

static VALUE
rip_get_line_gets(parser)
    struct ruby_parser *parser;
{
    return rb_funcall(parser->src, rip_id_gets, 0);
}


static VALUE
rip_lex_getline(parser)
    struct ruby_parser *parser;
{
#ifdef DEBUG
    fprintf(stderr, "@ %d\n", parser->source_line + 1);
#endif
    return (*lex_gets)(parser);
}

static void
ripper_mark(ptr)
    void *ptr;
{
    struct ruby_parser *p = (struct ruby_parser *)ptr;
    rb_gc_mark(p->source_file);
    rb_gc_mark(p->src);
    rb_gc_mark(p->last_line);
}

static void
ripper_free(ptr)
    void *ptr;
{
    struct ruby_parser *p = (struct ruby_parser *)ptr;

    if (p->token_buffer) free(p->token_buffer);
    free(p);
}

static VALUE
ripper_s_new(argc, argv, klass)
    int argc;
    VALUE *argv;
    VALUE klass;
{
    struct ruby_parser *p;
    VALUE self;

    p = ALLOC_N(struct ruby_parser, 1);
    MEMZERO(p, struct ruby_parser, 1);
    self = Data_Wrap_Struct(klass, ripper_mark, ripper_free, p);
    p->value = self;
    rb_obj_call_init(self, argc, argv);
    return self;
}

static VALUE rip_do_parse _((VALUE));
static VALUE rip_ensure _((VALUE));
extern VALUE rb_thread_pass _((void));

struct rip_arg {
    struct ruby_parser *parser;
    int argc;
    VALUE *argv;
};

static VALUE
ripper_parse(argc, argv, self)
    int argc;
    VALUE *argv;
    VALUE self;
{
    struct ruby_parser *parser;
    struct rip_arg arg;

    Data_Get_Struct(self, struct ruby_parser, parser);

    while (parser->parsing) {
        rb_thread_pass();
    }
    parser->parsing = 1;
    arg.parser = parser;
    arg.argc = argc;
    arg.argv = argv;
    return rb_ensure(rip_do_parse, (VALUE)(&arg),
                     rip_ensure, (VALUE)parser);
}

static VALUE
rip_do_parse(v)
    VALUE v;
{
    struct rip_arg *arg = (struct rip_arg*)v;
    struct ruby_parser *parser = arg->parser;
    VALUE src, fname, lineno;

    rb_scan_args(arg->argc, arg->argv, "12", &src, &fname, &lineno);
    switch (TYPE(src)) {
    case T_STRING:
        parser->gets = rip_get_line_String;
        break;
    case T_FILE:
        parser->gets = rip_get_line_File;
        break;
    default:
        if (! rb_respond_to(src, rip_id_gets)) {
            rb_raise(rb_eTypeError, "arg must be a String/File or respond to gets");
        }
        parser->gets = rip_get_line_gets;
        break;
    }
    parser->src = src;
    parser->source_file = NIL_P(fname) ? rb_str_new2("(ripper)") : fname;
    parser->source_line = NIL_P(lineno) ? 0 : NUM2INT(lineno) - 1;

    parser->ptr_beg = 0;
    parser->ptr     = 0;
    parser->ptr_end = 0;

    parser->result            = Qnil;

    parser->__end__seen       = 0;
    parser->heredocument_end  = 0;
    parser->condition_stack   = 0;
    parser->commandarg_stack  = 0;
    parser->rip_command_start = 1;
    parser->class_nesting     = 0;
    parser->in_method         = 0;
    parser->in_singleton      = 0;
    parser->in_defined_arg    = 0;

    parser->current_method_id = Qnil;
    parser->last_token_id     = Qnil;

    rip_yyparse((void*)parser);
    return parser->result;
}

static VALUE
rip_ensure(v)
    VALUE v;
{
    struct ruby_parser *parser = (struct ruby_parser*)v;
    parser->parsing = 0;
    return Qnil;
}

static VALUE
ripper_pointer(self)
    VALUE self;
{
    struct ruby_parser *parser;

    Data_Get_Struct(self, struct ruby_parser, parser);
    if (!parser->parsing) {
        return Qnil;
    }
    else {
        return INT2FIX(parser->ptr - parser->ptr_beg);
    }
}

static VALUE
rrb_ripper_lineno(self)
     VALUE self;
{
    struct ruby_parser *parser;

    Data_Get_Struct(self, struct ruby_parser, parser);
    if (!parser->parsing) {
        return Qnil;
    }
    else {
        return INT2FIX(parser->source_line);
    }
}
 

#include "dispids.c"

void
Init_rrb_ripper()
{
    VALUE Ripper;

    Ripper = rb_define_class("Ripper", rb_cObject);
    rb_define_const(Ripper, "Version", rb_str_new2(RIPPER_VERSION));
    rb_define_singleton_method(Ripper, "new", ripper_s_new, -1);
    rb_define_method(Ripper, "parse", ripper_parse, -1);
    rb_define_method(Ripper, "pointer", ripper_pointer, 0);
    rb_define_method(Ripper, "lineno", rrb_ripper_lineno, 0);

    rip_id_gets = rb_intern("gets");
    rip_init_dispatch_ids();

    /* ensure existing in symbol table */
    rb_intern("||");
    rb_intern("&&");
}

static VALUE
rip_id2sym(id)
    ID id;
{
    char *name;
    char buf[8];

    if (id <= 256) {
        buf[0] = id;
        buf[1] = '\0';
        return ID2SYM(rb_intern(buf));
    }
    switch (id) {
    case tOROP:
        name = "||";
        break;
    case tANDOP:
        name = "&&";
        break;
    default:
        name = rb_id2name(id);
        break;
    }
    if (!name) {
        rb_bug("cannot convert system ID to string: %ld", (unsigned long)id);
    }
    return ID2SYM(rb_intern(name));
}

static VALUE
rip_tok2sym(s)
    char *s;
{
    return ID2SYM(rb_intern(s));
}

static inline int
rip_nextc(parser)
    struct ruby_parser *parser;
{
    int c;

    if (lex_p == lex_pend) {
	if (lex_input) {
	    VALUE v = lex_getline();

	    if (NIL_P(v)) return -1;
	    if (heredoc_end > 0) {
		ruby_sourceline = heredoc_end;
		heredoc_end = 0;
	    }
	    ruby_sourceline++;
	    lex_pbeg = lex_p = RSTRING(v)->ptr;
	    lex_pend = lex_p + RSTRING(v)->len;
	    if (strncmp(lex_pbeg, "__END__", 7) == 0 &&
		(RSTRING(v)->len == 7 || lex_pbeg[7] == '\n' || lex_pbeg[7] == '\r')) {
		ruby__end__seen = 1;
		lex_lastline = 0;
		return -1;
	    }
	    lex_lastline = v;
	}
	else {
	    lex_lastline = 0;
	    return -1;
	}
    }
    c = (unsigned char)*lex_p++;
    if (c == '\r' && lex_p <= lex_pend && *lex_p == '\n') {
	lex_p++;
	c = '\n';
    }

    return c;
}

static void
rip_pushback(parser, c)
    struct ruby_parser *parser;
    int c;
{
    if (c == -1) return;
    lex_p--;
}

#define peek(c) (lex_p != lex_pend && (c) == *lex_p)

#define tokfix()   (tokenbuf[tokidx]='\0')
#define tok()      tokenbuf
#define toklen()   tokidx
#define toklast()  (tokidx>0?tokenbuf[tokidx-1]:0)

static void
rip_newtok(parser)
    struct ruby_parser *parser;
{
    tokidx = 0;
    if (!tokenbuf) {
        toksiz = 60;
        tokenbuf = ALLOC_N(char, 60);
    }
    if (toksiz > 4096) {
        toksiz = 60;
        REALLOC_N(tokenbuf, char, 60);
    }
}

static void
rip_tokadd(parser, c)
    struct ruby_parser *parser;
    char c;
{
    tokenbuf[tokidx++] = c;
    if (tokidx >= toksiz) {
	toksiz += 60;
        REALLOC_N(tokenbuf, char, toksiz);
    }
}

static int
rip_read_escape(parser)
    struct ruby_parser *parser;
{
    int c;

    switch (c = nextc()) {
      case '\\':	/* Backslash */
	return c;

      case 'n':	/* newline */
	return '\n';

      case 't':	/* horizontal tab */
	return '\t';

      case 'r':	/* carriage-return */
	return '\r';

      case 'f':	/* form-feed */
	return '\f';

      case 'v':	/* vertical tab */
	return '\13';

      case 'a':	/* alarm(bell) */
	return '\007';

      case 'e':	/* escape */
	return 033;

      case '0': case '1': case '2': case '3': /* octal constant */
      case '4': case '5': case '6': case '7':
	{
	    char buf[3];
	    int i;

	    pushback(c);
	    for (i=0; i<3; i++) {
		c = nextc();
		if (c == -1) goto eof;
		if (c < '0' || '7' < c) {
		    pushback(c);
		    break;
		}
		buf[i] = c;
	    }
	    c = scan_oct(buf, i, &i);
	}
	return c;

      case 'x':	/* hex constant */
	{
	    int numlen;

	    c = scan_hex(lex_p, 2, &numlen);
	    lex_p += numlen;
	}
	return c;

      case 'b':	/* backspace */
	return '\010';

      case 's':	/* space */
	return ' ';

      case 'M':
	if ((c = nextc()) != '-') {
	    yyerror("Invalid escape character syntax");
	    pushback(c);
	    return '\0';
	}
	if ((c = nextc()) == '\\') {
	    return read_escape() | 0x80;
	}
	else if (c == -1) goto eof;
	else {
	    return ((c & 0xff) | 0x80);
	}

      case 'C':
	if ((c = nextc()) != '-') {
	    yyerror("Invalid escape character syntax");
	    pushback(c);
	    return '\0';
	}
      case 'c':
	if ((c = nextc())== '\\') {
	    c = read_escape();
	}
	else if (c == '?')
	    return 0177;
	else if (c == -1) goto eof;
	return c & 0x9f;

      eof:
      case -1:
        yyerror("Invalid escape character syntax");
	return '\0';

      default:
	return c;
    }
}

static int
rip_tokadd_escape(parser, term)
    struct ruby_parser *parser;
    int term;
{
    int c;

    switch (c = nextc()) {
      case '\n':
	return 0;		/* just ignore */

      case '0': case '1': case '2': case '3': /* octal constant */
      case '4': case '5': case '6': case '7':
	{
	    int i;

	    tokadd('\\');
	    tokadd(c);
	    for (i=0; i<2; i++) {
		c = nextc();
		if (c == -1) goto eof;
		if (c < '0' || '7' < c) {
		    pushback(c);
		    break;
		}
		tokadd(c);
	    }
	}
	return 0;

      case 'x':	/* hex constant */
	{
	    int numlen;

	    tokadd('\\');
	    tokadd(c);
	    scan_hex(lex_p, 2, &numlen);
	    while (numlen--)
		tokadd(nextc());
	}
	return 0;

      case 'M':
	if ((c = nextc()) != '-') {
	    yyerror("Invalid escape character syntax");
	    pushback(c);
	    return 0;
	}
	tokadd('\\'); tokadd('M'); tokadd('-');
	goto escaped;

      case 'C':
	if ((c = nextc()) != '-') {
	    yyerror("Invalid escape character syntax");
	    pushback(c);
	    return 0;
	}
	tokadd('\\'); tokadd('C'); tokadd('-');
	goto escaped;

      case 'c':
	tokadd('\\'); tokadd('c');
      escaped:
	if ((c = nextc()) == '\\') {
	    return tokadd_escape(term);
	}
	else if (c == -1) goto eof;
	tokadd(c);
	return 0;

      eof:
      case -1:
        yyerror("Invalid escape character syntax");
	return -1;

      default:
        if (c != '/' || c != term)
            tokadd('\\');
	tokadd(c);
    }
    return 0;
}

static VALUE
rb_str_new_from_char(c)
    int c;
{
    char buf[8];
    buf[0] = (char)c;
    buf[1] = '\0';
    return rb_str_new2(buf);
}

static int
rip_parse_regx(parser, context, term, paren)
    struct ruby_parser *parser;
    VALUE context;
    int term, paren;
{
    register int c;
    char kcode = 0;
    int once = 0;
    int nest = 0;
    int options = 0;
    int re_start = ruby_sourceline;

    newtok();
    while ((c = nextc()) != -1) {
	if (c == term && nest == 0) {
	    goto regx_end;
	}

	switch (c) {
	  case '#':
            if (context == Qundef) {
                context = dispatch0(new_regexp);
            }
            if (! str_extend(context, term)) return 0;
	    continue;

	  case '\\':
	    if (tokadd_escape(term) < 0)
		return 0;
	    continue;

	  case -1:
	    goto unterminated;

	  default:
	    if (paren)  {
	      if (c == paren) nest++;
	      if (c == term) nest--;
	    }
	    if (ismbchar(c)) {
		int i, len = mbclen(c)-1;

		for (i = 0; i < len; i++) {
		    tokadd(c);
		    c = nextc();
		}
	    }
	    break;

	  regx_end:
	    for (;;) {
		switch (c = nextc()) {
		  case 'i':
		    options |= RE_OPTION_IGNORECASE;
		    break;
		  case 'x':
		    options |= RE_OPTION_EXTENDED;
		    break;
		  case 'p':	/* /p is obsolete */
		    rb_warn("/p option is obsolete; use /m\n\tnote: /m does not change ^, $ behavior");
		    options |= RE_OPTION_POSIXLINE;
		    break;
		  case 'm':
		    options |= RE_OPTION_MULTILINE;
		    break;
		  case 'o':
		    once = 1;
		    break;
		  case 'n':
		    kcode = 16;
		    break;
		  case 'e':
		    kcode = 32;
		    break;
		  case 's':
		    kcode = 48;
		    break;
		  case 'u':
		    kcode = 64;
		    break;
		  default:
		    pushback(c);
		    goto end_options;
		}
	    }

	  end_options:
	    tokfix();
	    lex_state = EXPR_END;
	    if (context != Qundef) {
                dispatch2(set_line, context, INT2NUM(re_start));
		if (toklen() > 0) {
                    s_dispatch2(add_string, context, rb_str_new(tok(),toklen()));
		}
                if (once)
                    dispatch1(is_once_regexp, context);
                else
                    dispatch1(is_dyna_regexp, context);
                dispatch2(regexp_options, context, INT2NUM(options | kcode));
                s_dispatch2(regexp_end, context, rb_str_new_from_char(term));
		return tDREGEXP;
	    }
	    else {
                yylval.val =
                context    = s_dispatch1(new_regexp, rb_str_new(tok(), toklen()));
                dispatch2(regexp_options, context, INT2NUM(options | kcode));
                s_dispatch2(regexp_end, context, rb_str_new_from_char(term));
		return tREGEXP;
	    }
	}
	tokadd(c);
    }
  unterminated:
    ruby_sourceline = re_start;
    rb_compile_error("unterminated regexp meets end of file");
    return 0;
}

static int
rip_parse_string(parser, context, func, term, paren)
    struct ruby_parser *parser;
    VALUE context;
    int func, term, paren;
{
    int c;
    int strstart;
    int nest = 0;

    if (func == '\'') {
	return parse_qstring(context, term, paren);
    }
    if (func == 0) {		/* read 1 line for heredoc */
				/* -1 for chomp */
	yylval.val = rb_str_new(lex_pbeg, lex_pend - lex_pbeg - 1);
	lex_p = lex_pend;
	return tSTRING;
    }
    if (context == Qundef) {
        context = dispatch0(new_string);
    }
    strstart = ruby_sourceline;
    newtok();
    while ((c = nextc()) != term || nest > 0) {
	if (c == -1) {
	  unterm_str:
	    ruby_sourceline = strstart;
	    rb_compile_error("unterminated string meets end of file");
	    return 0;
	}
	if (ismbchar(c)) {
	    int i, len = mbclen(c)-1;

	    for (i = 0; i < len; i++) {
		tokadd(c);
		c = nextc();
	    }
	}
	else if (c == '#') {
            if (context == Qundef) {
                context = dispatch0(new_string);
            }
            if (! str_extend(context, term)) goto unterm_str;
	    continue;
	}
	else if (c == '\\') {
	    c = nextc();
	    if (c == '\n')
		continue;
	    if (c == term) {
		tokadd(c);
	    }
	    else {
                pushback(c);
                if (func != '"') tokadd('\\');
                tokadd(read_escape());
  	    }
	    continue;
	}
	if (paren) {
	    if (c == paren) nest++;
	    if (c == term && nest-- == 0) break;
	}
	tokadd(c);
    }

    tokfix();
    lex_state = EXPR_END;

    if (context != Qundef) {
        dispatch2(set_line, context, INT2NUM(strstart));
	if (toklen() > 0) {
            s_dispatch2(add_string, context, rb_str_new(tok(), toklen()));
	}
        s_dispatch2(string_end, context, rb_str_new_from_char(term));
	yylval.val = context;
	if (func == '`') {
            dispatch1(is_xstring, context);
	    return tDXSTRING;
	}
	else {
	    return tDSTRING;
	}
    }
    else {
	yylval.val =
        context    = dispatch0(new_string);
        s_dispatch2(add_string, context, rb_str_new(tok(), toklen()));
        s_dispatch2(string_end, context, rb_str_new_from_char(term));
        if (func == '`') {
            dispatch1(is_xstring, context);
            return tXSTRING;
        }
        else {
            return tSTRING;
        }
    }
}

static int
rip_parse_qstring(parser, context, term, paren)
    struct ruby_parser *parser;
    VALUE context;
    int term, paren;
{
    int strstart;
    int c;
    int nest = 0;

    strstart = ruby_sourceline;
    newtok();
    while ((c = nextc()) != term || nest > 0) {
	if (c == -1) {
	    ruby_sourceline = strstart;
	    rb_compile_error("unterminated string meets end of file");
	    return 0;
	}
	if (ismbchar(c)) {
	    int i, len = mbclen(c)-1;

	    for (i = 0; i < len; i++) {
		tokadd(c);
		c = nextc();
	    }
	}
	else if (c == '\\') {
	    c = nextc();
	    switch (c) {
	      case '\n':
		continue;

	      case '\\':
		c = '\\';
		break;

	      default:
		/* fall through */
		if (c == term || (paren && c == paren)) {
		    tokadd(c);
		    continue;
		}
		tokadd('\\');
	    }
	}
	if (paren) {
	    if (c == paren) nest++;
	    if (c == term && nest-- == 0) break;
	}
	tokadd(c);
    }

    tokfix();
    yylval.val = rb_str_new(tok(), toklen());
    s_dispatch2(add_string, context, yylval.val);
    s_dispatch2(string_end, context, rb_str_new_from_char(term));
    lex_state = EXPR_END;
    return tSTRING;
}

static int
rip_parse_quotedwords(parser, context, term, paren)
    struct ruby_parser *parser;
    VALUE context;
    int term, paren;
{
    int strstart;
    int c;
    int nest = 0;

    strstart = ruby_sourceline;
    newtok();

    while (c = nextc(),ISSPACE(c))
	tokadd(c);		/* skip preceding spaces */
    pushback(c);
    tokfix();
    if (toklen() > 0) {
        s_dispatch2(add_space, context, rb_str_new(tok(), toklen()));
    }
    newtok();
    while ((c = nextc()) != term || nest > 0) {
	if (c == -1) {
	    ruby_sourceline = strstart;
	    rb_compile_error("unterminated quoted words meets end of file");
	    return 0;
	}
	if (ismbchar(c)) {
	    int i, len = mbclen(c)-1;

	    for (i = 0; i < len; i++) {
		tokadd(c);
		c = nextc();
	    }
	}
	else if (c == '\\') {
	    c = nextc();
	    switch (c) {
	      case '\n':
		continue;
	      case '\\':
		c = '\\';
		break;
	      default:
		if (c == term || (paren && c == paren)) {
		    tokadd(c);
		    continue;
		}
		if (!ISSPACE(c))
		    tokadd('\\');
		break;
	    }
	}
	else if (ISSPACE(c)) {
	    tokfix();
            s_dispatch2(add_word, context, rb_str_new(tok(), toklen()));
	    newtok();
	    tokadd(c);
	    while (c = nextc(),ISSPACE(c))
		tokadd(c);    /* skip continuous spaces */
	    pushback(c);
            tokfix();
            s_dispatch2(word_space, context, rb_str_new(tok(), toklen()));
            newtok();
	    continue;
	}
	if (paren) {
	    if (c == paren) nest++;
	    if (c == term && nest-- == 0) break;
	}
	tokadd(c);
    }

    tokfix();
    if (toklen() > 0) {
        s_dispatch2(add_word, context, rb_str_new(tok(), toklen()));
    }
    s_dispatch2(words_end, context, rb_str_new_from_char(term));
    yylval.val = context;
    lex_state = EXPR_END;
    return tDSTRING;
}

static int
rip_parse_here_document(parser, term, indent)
    struct ruby_parser *parser;
    int term, indent;
{
    int c;
    volatile VALUE eos;
    volatile VALUE line = 0;
    char *p;
    VALUE context;
    VALUE str;
    VALUE save_last_line;
    int   save_line_offset;
    int   save_lineno = ruby_sourceline;
    char buf[8];
    char *bp;
    int orig_term;

    /* get context */
    switch (term) {
      case '"':
      case '\'':
      case '`':
        orig_term = term;
        break;
      default:
        orig_term = 0;
        break;
    }
    bp = buf;
    *bp++ = '<';
    *bp++ = '<';
    if (indent) *bp++ = '-';
    if (orig_term) *bp++ = orig_term;
    *bp++ = '\0';
    context = s_dispatch1(new_here_document, rb_str_new2(buf));
    
    /* get EOS */
    newtok();
    switch (term) {
      case '\'':
      case '"':
      case '`':
	while ((c = nextc()) != term) {
	    tokadd(c);
	}
	if (term == '\'') term = 0;
	break;

      default:
	c = term;
	term = '"';
	if (!is_identchar(c)) {
	    rb_warn("use of bare << to mean <<\"\" is deprecated");
	    break;
	}
	while (is_identchar(c)) {
	    tokadd(c);
	    c = nextc();
	}
	pushback(c);
	break;
    }
    tokfix();
    save_last_line = lex_lastline;
    save_line_offset = lex_p - lex_pbeg;
    eos = rb_str_new(tok(), toklen());
    s_dispatch2(here_document_eos, context, eos);
    if (orig_term) {
        s_dispatch2(here_document_eos_term, context, rb_str_new_from_char(term));
    }

    /* search EOS with adding contents */
    str = rb_str_new(0,0);
    for (;;) {
        /* check if this line is EOS */
	lex_lastline = line = lex_getline();
	if (NIL_P(line)) {
	  error:
	    ruby_sourceline = save_lineno;
	    rb_compile_error("can't find string \"%s\" anywhere before EOF",
                             RSTRING(eos)->ptr);
	    return 0;
	}
	ruby_sourceline++;
	p = RSTRING(line)->ptr;
	if (indent) {
	    while (*p && (*p == ' ' || *p == '\t')) {
		p++;
	    }
	}
	if (strncmp(RSTRING(eos)->ptr, p, RSTRING(eos)->len) == 0) {
	    if (p[RSTRING(eos)->len] == '\n' ||
                p[RSTRING(eos)->len] == '\r')
		break;
	    if (RSTRING(eos)->len == RSTRING(line)->len)
		break;
	}

        /* rewind pointer to the begging of line */
	lex_pbeg =
        lex_p    = RSTRING(line)->ptr;
	lex_pend = lex_p + RSTRING(line)->len;
#if 0
	if (indent) {
	    while (*lex_p && *lex_p == '\t') {
		lex_p++;
	    }
	}
#endif
        
      retry:
        /* add line as content */
	if (! parse_string(context, term, '\n', '\n'))
            goto error;
        dispatch2(add_string, context, rb_str_new("\n", 1));
	if (lex_p != lex_pend)
	    goto retry;
    }
    dispatch1(here_document_end, context);
    lex_state = EXPR_END;

    /* resume to original (start of here-document) line */
    lex_lastline = save_last_line;
    lex_pbeg = RSTRING(save_last_line)->ptr;
    lex_pend = lex_pbeg + RSTRING(save_last_line)->len;
    lex_p = lex_pbeg + save_line_offset;

    heredoc_end = ruby_sourceline;
    ruby_sourceline = save_lineno;

    if (context != Qundef) {   /* always true ... is this good? */
        dispatch2(set_line, context, INT2NUM(save_lineno + 1));
	yylval.val = context;
    }
    switch (term) {
      case '\0':
      case '\'':
      case '"':
        /* still moving ... */
	if (context != Qundef) return tDSTRING;
	yylval.val = context; /* yylval.val = str; */
	return tSTRING;
      case '`':
        dispatch1(is_xstring, context);
	if (context != Qundef) return tDXSTRING;
	yylval.val = context; /* yylval.val = str; */
        return tXSTRING;
    }
    return 0;
}

#define BRACE_STR_EXTEND_TYPE 0
#define NO_BRACE_STR_EXTEND_TYPE 1
/* read one #{...} #$gvar #@ivar in string/regexp */
static int
rip_str_extend(parser, context, term)
    struct ruby_parser *parser;
    VALUE context;
    char term;
{
    int c;
    int brace = -1;
    int nest;
    int type;

    c = nextc();
    switch (c) {
      case '$':
      case '@':
      case '{':
	break;
      default:
	tokadd('#');
	pushback(c);
	return 1;
    }

    /* flush buffer (is normal string/regexp) */
    if (toklen() > 0) {
        s_dispatch2(add_string, context, rb_str_new(tok(), toklen()));
        newtok();
    }

    switch (c) {
      case '$':
	type = NO_BRACE_STR_EXTEND_TYPE;
	tokadd('$');
	c = nextc();
	if (c == -1) return 0;
	switch (c) {
	  case '1': case '2': case '3':
	  case '4': case '5': case '6':
	  case '7': case '8': case '9':
            /* backref */
	    while (ISDIGIT(c)) {
		tokadd(c);
		c = nextc();
	    }
	    pushback(c);
	    goto fetch_id;

	  case '&': case '+':
	  case '_': case '~':
	  case '*': case '$': case '?':
	  case '!': case '@': case ',':
	  case '.': case '=': case ':':
	  case '<': case '>': case '\\':
	  refetch:
            /* special gvar */
	    tokadd(c);
	    goto fetch_id;

          default:
	    if (c == term) {
                s_dispatch2(add_string, context, rb_str_new2("#$"));
		pushback(c);
		newtok();
		return 0;
	    }
	    switch (c) {   /* cannot check one switch; might be term */
	      case '\"':
	      case '/':
	      case '\'':
	      case '`':
		goto refetch;
	    }
	    if (!is_identchar(c)) {
		yyerror("bad global variable in string");
		newtok();
		return 0;
	    }
	}

        /* normal gvar */
	while (is_identchar(c)) {
	    tokadd(c);
	    if (ismbchar(c)) {
		int i, len = mbclen(c)-1;

		for (i = 0; i < len; i++) {
		    c = nextc();
		    tokadd(c);
		}
	    }
	    c = nextc();
	}
	pushback(c);
	break;

      case '@':
        /* ivar & cvar */
	type = NO_BRACE_STR_EXTEND_TYPE;
	tokadd(c);
	c = nextc();
        if (c == '@') {
            /* cvar */
	    tokadd(c);
	    c = nextc();
        }
	while (is_identchar(c)) {
	    tokadd(c);
	    if (ismbchar(c)) {
		int i, len = mbclen(c)-1;

		for (i = 0; i < len; i++) {
		    c = nextc();
		    tokadd(c);
		}
	    }
	    c = nextc();
	}
	pushback(c);
	break;

      case '{':
	if (c == '{') brace = '}';
	type = BRACE_STR_EXTEND_TYPE;
	nest = 0;
	do {
	  loop_again:
	    c = nextc();
	    switch (c) {
	      case -1:
		if (nest > 0) {
		    yyerror("bad substitution in string");
		    newtok();
		    return 1;
		}
		return 0;
	      case '}':
		if (c == brace) {
		    if (nest == 0) break;
		    nest--;
		}
		tokadd(c);
		goto loop_again;
	      case '\\':
		c = nextc();
		if (c == -1) return 0;
		if (c == term) {
		    tokadd(c);
		}
		else {
		    tokadd('\\');
		    tokadd(c);
		}
		break;
	      case '{':
		if (brace != -1) nest++;
	      case '\"':
	      case '/':
	      case '`':
		if (c == term) {
                    /* unterminated #{...} block: add as string */
		    pushback(c);
                    s_dispatch2(add_string, context, rb_str_new2("#"));
		    rb_warning("bad substitution in string");
		    tokfix();
		    s_dispatch2(add_string, context, rb_str_new(tok(), toklen()));
		    newtok();
		    return 1;
		}
	      default:
		tokadd(c);
		break;
	    }
	} while (c != brace);
    }

  fetch_id:
    tokfix();
    
    if( type == BRACE_STR_EXTEND_TYPE ){
      s_dispatch2(eval_string_begin, context, rb_str_new2("#{"));
      s_dispatch2(add_eval_string, context, rb_str_new(tok(), toklen()));
      s_dispatch2(eval_string_end, context, rb_str_new2("}"));
    }else{
      s_dispatch2(eval_string_begin, context, rb_str_new2("#"));
      s_dispatch2(add_eval_string, context, rb_str_new(tok(), toklen()));
      s_dispatch2(eval_string_end, context, rb_str_new2(""));
    }
    newtok();

    return 1;
}

#include "resword.c"
#define rb_reserved_word  rip_reserved_word

#define arg_ambiguous()  dispatch0(ambiguous_argument);

#if !defined(strtod) && !defined(HAVE_STDLIB_H)
double strtod ();
#endif

#define IS_ARG() (lex_state == EXPR_ARG || lex_state == EXPR_CMDARG)

static int rip_yylex0 _((struct ruby_parser *));

static int
rip_yylex(lval, parser_v)
    YYSTYPE *lval;
    void *parser_v;
{
    int t;
    struct ruby_parser *parser = (struct ruby_parser*)parser_v;

    parser->tmp_yylval = (void*)lval;
    t = rip_yylex0(parser);
    return t;
}

static int
rip_yylex0(parser)
    struct ruby_parser *parser;
{
#define last_id  (parser->last_token_id)
    register int c;
    int space_seen = 0;
    int cmd_state;
    VALUE context;

    cmd_state = command_start;
    command_start = Qfalse;

  retry:
    switch (c = nextc()) {
      case '\0':		/* NUL */
      case '\004':		/* ^D */
      case '\032':		/* ^Z */
      case -1:			/* end of script. */
	return 0;

	/* white spaces */
      case ' ': case '\t': case '\f': case '\r':
      case '\13': /* '\v' */
	space_seen++;
        newtok();
        tokadd(c);
        while ((c = nextc())) {
            switch (c) {
              case ' ': case '\t': case '\f': case '\r':
              case '\13': /* '\v' */
                space_seen++;
                tokadd(c);
                break;
              default:
                pushback(c);
                tokfix();
                s_dispatch1(space, rb_str_new(tok(), toklen()));
                goto retry;
            }
        }
        tokfix();
        s_dispatch1(space, rb_str_new(tok(), toklen()));
        return 0;

      case '#':		/* it's a comment */
        s_dispatch1(comment_start, rb_str_new2("#"));
        newtok();
	while ((c = nextc()) != '\n') {
	    if (c == -1) {
                tokfix();
                s_dispatch1(comment, rb_str_new(tok(), toklen()));
		return 0;
            }
            tokadd(c);
	}
        tokfix();
        s_dispatch1(comment, rb_str_new(tok(), toklen()));
	/* fall through */
      case '\n':
	switch (lex_state) {
	  case EXPR_BEG:
	  case EXPR_FNAME:
	  case EXPR_DOT:
            s_dispatch1(ignored_newline, rb_str_new2("\n"));
	    goto retry;
	  default:
	    break;
	}
        command_start = Qtrue;
	lex_state = EXPR_BEG;
        s_dispatch1(newline, rb_str_new2("\n"));
	return '\n';

      case '*':
	if ((c = nextc()) == '*') {
	    lex_state = EXPR_BEG;
	    if (nextc() == '=') {
		yylval.val = rip_id2sym(tPOW);
                s_dispatch1(tPOW_ASSIGN, rb_str_new2("**="));
		return tOP_ASGN;
	    }
	    pushback(c);
            s_dispatch1(POW, rb_str_new2("**"));
	    return tPOW;
	}
	if (c == '=') {
	    yylval.val = ID2SYM('*');
	    lex_state = EXPR_BEG;
            s_dispatch1(MUL_ASSIGN, rb_str_new2("*="));
	    return tOP_ASGN;
	}
	pushback(c);
	if (IS_ARG() && space_seen && !ISSPACE(c)){
	    rb_warning("`*' interpreted as argument prefix");
            s_dispatch1(STAR, rb_str_new2("*"));
	    c = tSTAR;
	}
	else if (lex_state == EXPR_BEG || lex_state == EXPR_MID) {
            s_dispatch1(STAR, rb_str_new2("*"));
	    c = tSTAR;
	}
	else {
            s_dispatch1(MUL, rb_str_new2("*"));
	    c = '*';
	}
	lex_state = EXPR_BEG;
	return c;

      case '!':
	lex_state = EXPR_BEG;
	if ((c = nextc()) == '=') {
            s_dispatch1(NEQ, rb_str_new2("!="));
	    return tNEQ;
	}
	if (c == '~') {
            s_dispatch1(NMATCH, rb_str_new2("!~"));
	    return tNMATCH;
	}
	pushback(c);
        s_dispatch1(BANG, rb_str_new2("!"));
	return '!';

      case '=':
	if (lex_p == lex_pbeg + 1) {
	    /* skip embedded rd document */
	    if (strncmp(lex_p, "begin", 5) == 0 && ISSPACE(lex_p[5])) {
                s_dispatch1(embdoc_begin, rb_str_new2("=begin"));
                s_dispatch1(embdoc_begin_label,
                            rb_str_new(lex_pbeg + 6, lex_pend - lex_pbeg - 6));
		for (;;) {
		    lex_p = lex_pend;
		    c = nextc();
		    if (c == -1) {
			rb_compile_error("embedded document meets end of file");
			return 0;
		    }
		    if (c != '=') {
                        s_dispatch1(embdoc, rb_str_new(lex_pbeg, lex_pend - lex_pbeg));
                        continue;
                    }
		    if (strncmp(lex_p, "end", 3) == 0 &&
			(lex_p + 3 == lex_pend || ISSPACE(lex_p[3]))) {
                        s_dispatch1(embdoc_end, rb_str_new2("=end"));
                        s_dispatch1(embdoc_end_label,
                                    rb_str_new(lex_pbeg + 4, lex_pend - lex_pbeg - 4));
			break;
		    }
                    s_dispatch1(embdoc, rb_str_new(lex_pbeg, lex_pend - lex_pbeg));
		}
		lex_p = lex_pend;
		goto retry;
	    }
	}

	lex_state = EXPR_BEG;
	if ((c = nextc()) == '=') {
	    if ((c = nextc()) == '=') {
                s_dispatch1(EQQ, rb_str_new2("==="));
		return tEQQ;
	    }
	    pushback(c);
            s_dispatch1(EQ, rb_str_new2("=="));
	    return tEQ;
	}
	if (c == '~') {
            s_dispatch1(MATCH, rb_str_new2("=~"));
	    return tMATCH;
	}
	else if (c == '>') {
            s_dispatch1(ASSOC, rb_str_new2("=>"));
	    return tASSOC;
	}
	pushback(c);
        s_dispatch1(ASSIGN, rb_str_new2("="));
	return '=';

      case '<':
	c = nextc();
	if (c == '<' &&
	    lex_state != EXPR_END &&
            lex_state != EXPR_ENDARG &&
            lex_state != EXPR_CLASS &&
	    (!IS_ARG() || space_seen)) {
 	    int c2 = nextc();
	    int indent = 0;
	    if (c2 == '-') {
		indent = 1;
		c2 = nextc();
	    }
	    if (!ISSPACE(c2) && (strchr("\"'`", c2) || is_identchar(c2))) {
		return parse_here_document(c2, indent);
	    }
	    pushback(c2);
	}
	lex_state = EXPR_BEG;
	if (c == '=') {
	    if ((c = nextc()) == '>') {
                s_dispatch1(CMP, rb_str_new2("<=>"));
		return tCMP;
	    }
	    pushback(c);
            s_dispatch1(CMP, rb_str_new2("<="));
	    return tLEQ;
	}
	if (c == '<') {
	    if (nextc() == '=') {
		yylval.val = rip_id2sym(tLSHFT);
                s_dispatch1(CMP_ASSIGN, rb_str_new2("<<="));
		return tOP_ASGN;
	    }
	    pushback(c);
            s_dispatch1(LSHIFT, rb_str_new2("<<"));
	    return tLSHFT;
	}
	pushback(c);
        s_dispatch1(LT, rb_str_new2("<"));
	return '<';

      case '>':
	lex_state = EXPR_BEG;
	if ((c = nextc()) == '=') {
            s_dispatch1(GEQ, rb_str_new2(">="));
	    return tGEQ;
	}
	if (c == '>') {
	    if ((c = nextc()) == '=') {
		yylval.val = rip_id2sym(tRSHFT);
                s_dispatch1(RSHIFT_ASSIGN, rb_str_new2(">>="));
		return tOP_ASGN;
	    }
	    pushback(c);
            s_dispatch1(RSHIFT, rb_str_new2(">>"));
	    return tRSHFT;
	}
	pushback(c);
        s_dispatch1(GT, rb_str_new2(">"));
	return '>';

      case '"':
	context = s_dispatch1(new_string, rb_str_new2("\""));
	return parse_string(context,c,c,c);
      case '`':
	if (lex_state == EXPR_FNAME || lex_state == EXPR_DOT) {
            s_dispatch1(BACKQUOTE, rb_str_new2("`"));
            return c;
        }
	context = s_dispatch1(new_xstring, rb_str_new2("`"));
	return parse_string(context,c,c,c);

      case '\'':
        context = s_dispatch1(new_string, rb_str_new2("'"));
	return parse_qstring(context,c,0);

      case '?':
	if (lex_state == EXPR_END || lex_state == EXPR_ENDARG) {
	    lex_state = EXPR_BEG;
            s_dispatch1(Q, rb_str_new2("?"));
	    return '?';
	}
	c = nextc();
	if (c == -1) {
	    rb_compile_error("incomplete character syntax");
	    return 0;
	}
	if (IS_ARG() && ISSPACE(c)){
	    pushback(c);
	    lex_state = EXPR_BEG;
            s_dispatch1(Q, rb_str_new2("?"));
	    return '?';
	}
	if (c == '\\') {
	    c = read_escape();
	}
	c &= 0xff;
	yylval.val = INT2FIX(c);
	lex_state = EXPR_END;
        s_dispatch1(CHAR, rb_str_new2("?"));  /* FIXME */
	return tINTEGER;

      case '&':
	if ((c = nextc()) == '&') {
	    lex_state = EXPR_BEG;
	    if ((c = nextc()) == '=') {
		yylval.val = ID2SYM(tANDOP);
                s_dispatch1(ANDAND_ASSIGN, rb_str_new2("&&="));
		return tOP_ASGN;
	    }
	    pushback(c);
            s_dispatch1(ANDAND, rb_str_new2("&&"));
	    return tANDOP;
	}
	else if (c == '=') {
	    yylval.val = ID2SYM('&');
	    lex_state = EXPR_BEG;
            s_dispatch1(AND_ASSIGN, rb_str_new2("&="));
	    return tOP_ASGN;
	}
	pushback(c);
	if (IS_ARG() && space_seen && !ISSPACE(c)){
	    rb_warning("`&' interpreted as argument prefix");
            s_dispatch1(AMPER, rb_str_new2("&"));
	    c = tAMPER;
	}
	else if (lex_state == EXPR_BEG || lex_state == EXPR_MID) {
            s_dispatch1(AMPER, rb_str_new2("&"));
	    c = tAMPER;
	}
	else {
            s_dispatch1(AND, rb_str_new2("&"));
	    c = '&';
	}
	lex_state = EXPR_BEG;
	return c;

      case '|':
	lex_state = EXPR_BEG;
	if ((c = nextc()) == '|') {
	    if ((c = nextc()) == '=') {
		yylval.val = rip_id2sym(tOROP);
                s_dispatch1(OROR_ASSIGN, rb_str_new2("||="));
		return tOP_ASGN;
	    }
	    pushback(c);
            s_dispatch1(OROR, rb_str_new2("||"));
	    return tOROP;
	}
	else if (c == '=') {
	    yylval.val = ID2SYM('|');
            s_dispatch1(OR_ASSIGN, rb_str_new2("|="));
	    return tOP_ASGN;
	}
	pushback(c);
        s_dispatch1(OR, rb_str_new2("|"));
	return '|';

      case '+':
	c = nextc();
	if (lex_state == EXPR_FNAME || lex_state == EXPR_DOT) {
	    if (c == '@') {
                s_dispatch1(UPLUS, rb_str_new2("+@"));
		return tUPLUS;
	    }
	    pushback(c);
            s_dispatch1(PLUS, rb_str_new2("+"));
	    return '+';
	}
	if (c == '=') {
	    lex_state = EXPR_BEG;
	    yylval.val = ID2SYM('+');
            s_dispatch1(PLUS_ASSIGN, rb_str_new2("+="));
	    return tOP_ASGN;
	}
	if (lex_state == EXPR_BEG || lex_state == EXPR_MID ||
	    (IS_ARG() && space_seen && !ISSPACE(c))) {
	    if (IS_ARG()) arg_ambiguous();
	    lex_state = EXPR_BEG;
	    pushback(c);
	    if (ISDIGIT(c)) {
		c = '+';
                s_dispatch1(UPLUS, rb_str_new2("+@"));
		goto start_num;
	    }
            s_dispatch1(UPLUS, rb_str_new2("+@"));
	    return tUPLUS;
	}
	lex_state = EXPR_BEG;
	pushback(c);
        s_dispatch1(PLUS, rb_str_new2("+"));
	return '+';

      case '-':
	c = nextc();
	if (lex_state == EXPR_FNAME || lex_state == EXPR_DOT) {
	    if (c == '@') {
                s_dispatch1(UMINUS, rb_str_new2("-@"));
		return tUMINUS;
	    }
	    pushback(c);
            s_dispatch1(MINUS, rb_str_new2("-"));
	    return '-';
	}
	if (c == '=') {
	    lex_state = EXPR_BEG;
	    yylval.val = ID2SYM('-');
            s_dispatch1(MINUS_ASSIGN, rb_str_new2("-="));
	    return tOP_ASGN;
	}
	if (lex_state == EXPR_BEG || lex_state == EXPR_MID ||
	    (IS_ARG() && space_seen && !ISSPACE(c))) {
	    if (IS_ARG()) arg_ambiguous();
	    lex_state = EXPR_BEG;
	    pushback(c);
	    if (ISDIGIT(c)) {
		c = '-';
                s_dispatch1(UMINUS, rb_str_new2("-@"));
		goto start_num;
	    }
            s_dispatch1(UMINUS, rb_str_new2("-@"));
	    return tUMINUS;
	}
	lex_state = EXPR_BEG;
	pushback(c);
        s_dispatch1(MINUS, rb_str_new2("-"));
	return '-';

      case '.':
	lex_state = EXPR_BEG;
	if ((c = nextc()) == '.') {
	    if ((c = nextc()) == '.') {
                s_dispatch1(DOT3, rb_str_new2("..."));
		return tDOT3;
	    }
	    pushback(c);
            s_dispatch1(DOT2, rb_str_new2(".."));
	    return tDOT2;
	}
	pushback(c);
	if (!ISDIGIT(c)) {
	    lex_state = EXPR_DOT;
            s_dispatch1(DOT, rb_str_new2("."));
	    return '.';
	}
	c = '.';
	/* fall through */

      start_num:
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
	{
	    int is_float, seen_point, seen_e, seen_uc;

	    is_float = seen_point = seen_e = seen_uc = 0;
	    lex_state = EXPR_END;
	    newtok();
	    if (c == '-' || c == '+') {
		tokadd(c);
		c = nextc();
	    }
	    if (c == '0') {
		c = nextc();
		if (c == 'x' || c == 'X') {
		    /* hexadecimal */
		    c = nextc();
		    do {
			if (c == '_') {
			    seen_uc = 1;
			    continue;
			}
			if (!ISXDIGIT(c)) break;
			seen_uc = 0;
			tokadd(c);
		    } while ((c = nextc()));
		    pushback(c);
		    tokfix();
		    if (toklen() == 0) {
			yyerror("hexadecimal number without hex-digits");
		    }
		    else if (seen_uc) goto trailing_uc;
		    yylval.val = rb_cstr2inum(tok(), 16);
                    s_dispatch1(INTEGER_16, rb_str_new2(tok()));
		    return tINTEGER;
		}
		if (c == 'b' || c == 'B') {
		    /* binary */
		    c = nextc();
		    do {
			if (c == '_') {
			    seen_uc = 1;
			    continue;
			}
			if (c != '0'&& c != '1') break;
			seen_uc = 0;
			tokadd(c);
		    } while ((c = nextc()));
		    pushback(c);
		    tokfix();
		    if (toklen() == 0) {
			yyerror("numeric literal without digits");
		    }
		    else if (seen_uc) goto trailing_uc;
		    yylval.val = rb_cstr2inum(tok(), 2);
                    s_dispatch1(INTEGER_2, rb_str_new2(tok()));
		    return tINTEGER;
		}
		if ((c >= '0' && c <= '7') || c == '_') {
		    /* octal */
	            do {
			if (c == '_') {
			    seen_uc = 1;
			    continue;
			}
			if (c < '0' || c > '7') break;
			seen_uc = 0;
			tokadd(c);
		    } while ((c = nextc()));
		    pushback(c);
		    tokfix();
		    if (seen_uc) goto trailing_uc;
		    yylval.val = rb_cstr2inum(tok(), 8);
                    s_dispatch1(INTEGER_8, rb_str_new2(tok()));
		    return tINTEGER;
		}
		if (c > '7' && c <= '9') {
		    yyerror("Illegal octal digit");
		}
		else if (c == '.') {
		    tokadd('0');
		}
		else {
		    pushback(c);
		    yylval.val = INT2FIX(0);
                    s_dispatch1(INTEGER_10, rb_str_new2("0"));
		    return tINTEGER;
		}
	    }

	    for (;;) {
		switch (c) {
		  case '0': case '1': case '2': case '3': case '4':
		  case '5': case '6': case '7': case '8': case '9':
		    seen_uc = 0;
		    tokadd(c);
		    break;

		  case '.':
		    if (seen_point || seen_e) {
			goto decode_num;
		    }
		    else {
			int c0 = nextc();
			if (!ISDIGIT(c0)) {
			    pushback(c0);
			    goto decode_num;
			}
			c = c0;
		    }
		    tokadd('.');
		    tokadd(c);
		    is_float++;
		    seen_point++;
		    seen_uc = 0;
		    break;

		  case 'e':
		  case 'E':
		    if (seen_e) {
			goto decode_num;
		    }
		    tokadd(c);
		    seen_e++;
		    is_float++;
		    while ((c = nextc()) == '_')
			seen_uc = 1;
		    if (c == '-' || c == '+')
			tokadd(c);
		    else 
			continue;
		    break;

		  case '_':	/* `_' in number just ignored */
		    seen_uc = 1;
		    break;

		  default:
		    goto decode_num;
		}
		c = nextc();
	    }

	  decode_num:
	    pushback(c);
	    tokfix();
	    if (seen_uc) {
	      trailing_uc:
		yyerror("trailing `_' in number");
	    }
	    if (is_float) {
		double d = strtod(tok(), 0);
		if (errno == ERANGE) {
		    rb_warn("Float %s out of range", tok());
		    errno = 0;
		}
		yylval.val = rb_float_new(d);
		return tFLOAT;
	    }
	    yylval.val = rb_cstr2inum(tok(), 10);
            s_dispatch1(INTEGER_10, rb_str_new2(tok()));
	    return tINTEGER;
	}

      case ']':
      case '}':
      case ')':
        COND_LEXPOP();
        CMDARG_LEXPOP();
	lex_state = EXPR_END;
        switch (c) {
          case ']':
            s_dispatch1(RBRACKET, rb_str_new2("]"));
            break;
          case '}':
            s_dispatch1(RBRACE, rb_str_new2("}"));
            break;
          case ')':
            s_dispatch1(RPAREN, rb_str_new2(")"));
            break;
        }
	return c;

      case ':':
	c = nextc();
	if (c == ':') {
	    if (lex_state == EXPR_BEG ||  lex_state == EXPR_MID ||
		(IS_ARG() && space_seen)) {
		lex_state = EXPR_BEG;
                s_dispatch1(COLON3, rb_str_new2("::"));
		return tCOLON3;
	    }
	    lex_state = EXPR_DOT;
            s_dispatch1(COLON2, rb_str_new2("::"));
	    return tCOLON2;
	}
	pushback(c);
	if (lex_state == EXPR_END || lex_state == EXPR_ENDARG || ISSPACE(c)) {
	    lex_state = EXPR_BEG;
            s_dispatch1(COLON, rb_str_new2(":"));
	    return ':';
	}
	lex_state = EXPR_FNAME;
        s_dispatch1(SYMBEG, rb_str_new2(":"));
	return tSYMBEG;

      case '/':
	if (lex_state == EXPR_BEG || lex_state == EXPR_MID) {
            context = s_dispatch1(new_regexp, rb_str_new2("/"));
	    return parse_regx(context, '/', '/');
	}
	if ((c = nextc()) == '=') {
	    lex_state = EXPR_BEG;
	    yylval.val = ID2SYM('/');
            s_dispatch1(SLASH_ASSIGN, rb_str_new2("/="));
	    return tOP_ASGN;
	}
	pushback(c);
	if (IS_ARG() && space_seen) {
	    if (!ISSPACE(c)) {
		arg_ambiguous();
                context = s_dispatch1(new_regexp, rb_str_new2("/"));
		return parse_regx(context, '/', '/');
	    }
	}
	lex_state = EXPR_BEG;
        s_dispatch1(SLASH, rb_str_new2("/"));
	return '/';

      case '^':
	lex_state = EXPR_BEG;
	if ((c = nextc()) == '=') {
	    yylval.val = ID2SYM('^');
            s_dispatch1(HAT_ASSIGN, rb_str_new2("^="));
	    return tOP_ASGN;
	}
	pushback(c);
        s_dispatch1(HAT, rb_str_new2("^"));
	return '^';

      case ';':
        s_dispatch1(SEMICOLON, rb_str_new2(";"));
        command_start = Qtrue;
	lex_state = EXPR_BEG;
	return c;
        /* NOT fall through */
      case ',':
	lex_state = EXPR_BEG;
        s_dispatch1(COMMA, rb_str_new2(","));
	return c;

      case '~':
	if (lex_state == EXPR_FNAME || lex_state == EXPR_DOT) {
	    if ((c = nextc()) != '@') {
		pushback(c);
	    }
	}
	lex_state = EXPR_BEG;
        s_dispatch1(TILDE, rb_str_new2("~"));
	return '~';

      case '(':
	command_start = Qtrue;
	if (lex_state == EXPR_BEG || lex_state == EXPR_MID) {
	    c = tLPAREN;
	}
	else if (space_seen) {
	    if (lex_state == EXPR_CMDARG) {
                c = tLPAREN_ARG;
            }
            else if (lex_state == EXPR_ARG) {
                c = tLPAREN_ARG;
                yylval.val = last_id;
            }
	}
        COND_PUSH(0);
        CMDARG_PUSH(0);
	lex_state = EXPR_BEG;
        switch (c) {
          case tLPAREN:
            s_dispatch1(LPAREN, rb_str_new2("("));
            break;
          case tLPAREN_ARG:
            s_dispatch1(LPAREN_ARG, rb_str_new2("("));
            break;
          default:
            s_dispatch1(LPAREN, rb_str_new2("("));
            break;
        }
	return c;

      case '[':
	if (lex_state == EXPR_FNAME || lex_state == EXPR_DOT) {
	    if ((c = nextc()) == ']') {
		if ((c = nextc()) == '=') {
                    s_dispatch1(ASET, rb_str_new2("[]="));
		    return tASET;
		}
		pushback(c);
                s_dispatch1(AREF, rb_str_new2("[]"));
		return tAREF;
	    }
	    pushback(c);
            s_dispatch1(LBRACKET, rb_str_new2("["));
	    return '[';
	}
	else if (lex_state == EXPR_BEG || lex_state == EXPR_MID) {
	    c = tLBRACK;
	}
	else if (IS_ARG() && space_seen) {
	    c = tLBRACK;
	}
        COND_PUSH(0);
        CMDARG_PUSH(0);
	lex_state = EXPR_BEG;
        s_dispatch1(LBRACKET, rb_str_new2("["));
	return c;

      case '{':
	if (!IS_ARG()) {
            if (space_seen && lex_state == EXPR_ENDARG)
                c = tLBRACE_ARG;
            if (lex_state != EXPR_END && lex_state != EXPR_ENDARG)
                c = tLBRACE;
        }
        COND_PUSH(0);
        CMDARG_PUSH(0);
	lex_state = EXPR_BEG;
        switch (c) {
          case tLBRACE_ARG:
            s_dispatch1(LBRACE_ARG, rb_str_new2("{"));
            break;
          case tLBRACE:
            s_dispatch1(LBRACE, rb_str_new2("{"));
            break;
          default:
            s_dispatch1(LBRACE, rb_str_new2("{"));
            break;
        }
	return c;

      case '\\':
	c = nextc();
	if (c == '\n') {
	    space_seen = 1;
            s_dispatch1(escaped_newline, rb_str_new2("\\\n"));
	    goto retry; /* skip \\n */
	}
	pushback(c);
        s_dispatch1(BACKSLASH, rb_str_new2("\\"));
	return '\\';

      case '%':
	if (lex_state == EXPR_BEG || lex_state == EXPR_MID) {
	    int term;
	    int paren;
#ifdef RIPPER
            char buf[8];
            char *p;
            int orig_c;
#endif

            c = nextc();
	  quotation:
            orig_c = c;
	    if (!ISALNUM(c)) {
		term = c;
		c = 'Q';
	    }
	    else {
		term = nextc();
	    }
	    if (c == -1 || term == -1) {
		rb_compile_error("unterminated quoted string meets end of file");
		return 0;
	    }
	    paren = term;
	    if (term == '(') term = ')';
	    else if (term == '[') term = ']';
	    else if (term == '{') term = '}';
	    else if (term == '<') term = '>';
	    else paren = 0;

#ifdef RIPPER
            p = buf;
            *p++ = '%';
            if (c == orig_c) *p++ = c;
            *p++ = paren ? paren : term;
            *p++ = '\0';
#endif
	    switch (c) {
	      case 'Q':
                context = s_dispatch1(new_string, rb_str_new2(buf));
		return parse_string(context, '"', term, paren);

	      case 'q':
                context = s_dispatch1(new_string, rb_str_new2(buf));
		return parse_qstring(context, term, paren);

	      case 'w':
                context = s_dispatch1(new_words, rb_str_new2(buf));
		return parse_quotedwords(context, term, paren);

	      case 'x':
                context = s_dispatch1(new_xstring, rb_str_new2(buf));
		return parse_string(context, '`', term, paren);

	      case 'r':
                context = s_dispatch1(new_regexp, rb_str_new2(buf));
		return parse_regx(context, term, paren);

	      default:
		yyerror("unknown type of %string");
		return 0;
	    }
	}
	if ((c = nextc()) == '=') {
	    yylval.val = ID2SYM('%');
            s_dispatch1(PERCENT_ASSIGN, rb_str_new2("%="));
	    return tOP_ASGN;
	}
	if (IS_ARG() && space_seen && !ISSPACE(c)) {
	    goto quotation;
	}
	lex_state = EXPR_BEG;
	pushback(c);
        s_dispatch1(PERCENT, rb_str_new2("%"));
	return '%';

      case '$':
	lex_state = EXPR_END;
	newtok();
	c = nextc();
	switch (c) {
	  case '_':		/* $_: last read line string */
	    c = nextc();
	    if (is_identchar(c)) {
		tokadd('$');
		tokadd('_');
		break;
	    }
	    pushback(c);
	    c = '_';
	    /* fall through */
	  case '~':		/* $~: match-data */
	    local_cnt(rip_tok2sym("~"));
	    /* fall through */
	  case '*':		/* $*: argv */
	  case '$':		/* $$: pid */
	  case '?':		/* $?: last status */
	  case '!':		/* $!: error string */
	  case '@':		/* $@: error position */
	  case '/':		/* $/: input record separator */
	  case '\\':		/* $\: output record separator */
	  case ';':		/* $;: field separator */
	  case ',':		/* $,: output field separator */
	  case '.':		/* $.: last read line number */
	  case '=':		/* $=: ignorecase */
	  case ':':		/* $:: load path */
	  case '<':		/* $<: reading filename */
	  case '>':		/* $>: default output handle */
	  case '\"':		/* $": already loaded files */
	    tokadd('$');
	    tokadd(c);
	    tokfix();
	    yylval.val = rip_tok2sym(tok());
            s_dispatch1(GVAR, rb_str_new2(tok()));
	    return tGVAR;

	  case '-':
	    tokadd('$');
	    tokadd(c);
	    c = nextc();
	    tokadd(c);
	    tokfix();
	    yylval.val = rip_tok2sym(tok());
	    /* xxx shouldn't check if valid option variable */
            s_dispatch1(GVAR, rb_str_new2(tok()));
	    return tGVAR;

	  case '&':		/* $&: last match */
	  case '`':		/* $`: string before last match */
	  case '\'':		/* $': string after last match */
	  case '+':		/* $+: string matches last paren. */
            {
                char buf[8];
                buf[0] = '$';
                buf[1] = c;
                buf[2] = '\0';
                yylval.val = rip_tok2sym(buf);
                s_dispatch1(BACK_REF, rb_str_new2(buf));
                return tBACK_REF;
            }

	  case '1': case '2': case '3':
	  case '4': case '5': case '6':
	  case '7': case '8': case '9':
	    tokadd('$');
	    while (ISDIGIT(c)) {
		tokadd(c);
		c = nextc();
	    }
	    if (is_identchar(c))
		break;
	    pushback(c);
	    tokfix();
	    yylval.val = rip_tok2sym(tok());
            s_dispatch1(NTH_REF, rb_str_new2(tok()));
	    return tNTH_REF;

	  default:
	    if (!is_identchar(c)) {
		pushback(c);
                /* s_dispatch1(DOLLER, rb_str_new2("$"));  no need? */
		return '$';
	    }
            /* FALLTHRU ?? */
	  case '0':
	    tokadd('$');
	}
	break;

      case '@':
	c = nextc();
	newtok();
	tokadd('@');
	if (c == '@') {
	    tokadd('@');
	    c = nextc();
	}
	if (ISDIGIT(c)) {
	    rb_compile_error("`@%c' is not a valid instance variable name", c);
	}
	if (!is_identchar(c)) {
	    pushback(c);
            /* s_dispatch1(AT, rb_str_new2("@"));  no need? */
	    return '@';
	}
	break;

      default:
	if (!is_identchar(c) || ISDIGIT(c)) {
	    rb_compile_error("Invalid char `\\%03o' in expression", c);
	    goto retry;
	}

	newtok();
	break;
    }

    while (is_identchar(c)) {
	tokadd(c);
	if (ismbchar(c)) {
	    int i, len = mbclen(c)-1;

	    for (i = 0; i < len; i++) {
		c = nextc();
		tokadd(c);
	    }
	}
	c = nextc();
    }
    if ((c == '!' || c == '?') && is_identchar(tok()[0]) && !peek('=')) {
	tokadd(c);
    }
    else {
	pushback(c);
    }
    tokfix();

    {
	int result = 0;

	switch (tok()[0]) {
	  case '$':
	    lex_state = EXPR_END;
	    result = tGVAR;
	    break;
	  case '@':
	    lex_state = EXPR_END;
	    if (tok()[1] == '@')
		result = tCVAR;
	    else
		result = tIVAR;
	    break;

	  default:
            if (toklast() == '!' || toklast() == '?') {
                result = tFID;
            }
            else {
                if (lex_state == EXPR_FNAME) {
                    if ((c = nextc()) == '=' && !peek('~') && !peek('>') &&
                        (!peek('=') || (lex_p + 1 < lex_pend && lex_p[1] == '>'))) {
                        result = tIDENTIFIER;
                        tokadd(c);
                    }
                    else {
                        pushback(c);
                    }
                }
                if (result == 0 && ISUPPER(tok()[0])) {
                    result = tCONSTANT;
                }
                else {
                    result = tIDENTIFIER;
                }
            }

	    if (lex_state != EXPR_DOT) {
                struct kwtable *kw;

		/* See if it is a reserved word.  */
		kw = rb_reserved_word(tok(), toklen());
		if (kw) {
		    enum lex_state_e state = lex_state;
		    lex_state = kw->state;
		    /*
                    if (state == EXPR_FNAME) {
                        yylval.val = rip_tok2sym(kw->name);
		    }
                    */
                    yylval.val = s_dispatch1(KEYWORD, rb_str_new2(kw->name));
		    if (kw->id1 == kDO) {
			if (COND_P()) return kDO_COND;
			if (CMDARG_P() && state != EXPR_CMDARG)
                            return kDO_BLOCK;
			return kDO;
		    }
		    if (state == EXPR_BEG)
			return kw->id1;
		    else {
			if (kw->id1 != kw->id2)
			    lex_state = EXPR_BEG;
			return kw->id2;
		    }
		}
	    }

	    if (lex_state == EXPR_BEG ||
		lex_state == EXPR_DOT ||
		lex_state == EXPR_ARG ||
		lex_state == EXPR_CMDARG) {
                if (cmd_state)
                    lex_state = EXPR_CMDARG;
                else
                    lex_state = EXPR_ARG;
	    }
	    else {
		lex_state = EXPR_END;
	    }
	}
	tokfix();
	last_id = yylval.val = rip_tok2sym(tok());
        switch (result) {
          case tIDENTIFIER:
	    last_id = yylval.val =
	      s_dispatch1(IDENTIFIER, rb_str_new2(tok()));
            break;
          case tFID:
	    last_id = yylval.val =
	      s_dispatch1(FID, rb_str_new2(tok()));
            break;
          case tCVAR:
	    last_id = yylval.val =
	      s_dispatch1(CVAR, rb_str_new2(tok()));
            break;
          case tIVAR:
	    last_id = yylval.val =
	      s_dispatch1(IVAR, rb_str_new2(tok()));
            break;
          case tGVAR:
	    last_id = yylval.val =
	      s_dispatch1(GVAR, rb_str_new2(tok()));
            break;
          case tCONSTANT:
	    last_id = yylval.val =
	      s_dispatch1(CONSTANT, rb_str_new2(tok()));
            break;
        }
	return result;
    }
}

#ifndef RIPPER
#  include "nodeop.c"
#endif

#ifndef RIPPER
#  include "lv.c"
#endif

#ifndef RIPPER
#  include "optnode.c"
#endif

#ifndef RIPPER
#  include "symbol_table.c"
#endif

#ifndef RIPPER
#  include "special_local.c"
#endif
