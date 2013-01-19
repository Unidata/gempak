
# line 21 "po-gram-gen.y"
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

/* Specification.  */
#include "po-gram.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "str-list.h"
#include "po-lex.h"
#include "po-charset.h"
#include "error.h"
#include "xalloc.h"
#include "gettext.h"
#include "read-catalog-abstract.h"

#define _(str) gettext (str)

/* Remap normal yacc parser interface names (yyparse, yylex, yyerror, etc),
   as well as gratuitiously global symbol names, so we can have multiple
   yacc generated parsers in the same program.  Note that these are only
   the variables produced by yacc.  If other parser generators (bison,
   byacc, etc) produce additional global names that conflict at link time,
   then those parser generators need to be fixed instead of adding those
   names to this list. */

#define yymaxdepth po_gram_maxdepth
#define yyparse po_gram_parse
#define yylex   po_gram_lex
#define yyerror po_gram_error
#define yylval  po_gram_lval
#define yychar  po_gram_char
#define yydebug po_gram_debug
#define yypact  po_gram_pact
#define yyr1    po_gram_r1
#define yyr2    po_gram_r2
#define yydef   po_gram_def
#define yychk   po_gram_chk
#define yypgo   po_gram_pgo
#define yyact   po_gram_act
#define yyexca  po_gram_exca
#define yyerrflag po_gram_errflag
#define yynerrs po_gram_nerrs
#define yyps    po_gram_ps
#define yypv    po_gram_pv
#define yys     po_gram_s
#define yy_yys  po_gram_yys
#define yystate po_gram_state
#define yytmp   po_gram_tmp
#define yyv     po_gram_v
#define yy_yyv  po_gram_yyv
#define yyval   po_gram_val
#define yylloc  po_gram_lloc
#define yyreds  po_gram_reds          /* With YYDEBUG defined */
#define yytoks  po_gram_toks          /* With YYDEBUG defined */
#define yylhs   po_gram_yylhs
#define yylen   po_gram_yylen
#define yydefred po_gram_yydefred
#define yydgoto po_gram_yydgoto
#define yysindex po_gram_yysindex
#define yyrindex po_gram_yyrindex
#define yygindex po_gram_yygindex
#define yytable  po_gram_yytable
#define yycheck  po_gram_yycheck

static long plural_counter;

#define check_obsolete(value1,value2) \
  if ((value1).obsolete != (value2).obsolete) \
    po_gram_error_at_line (&(value2).pos, _("inconsistent use of #~"));

static inline void
do_callback_message (char *msgctxt,
		     char *msgid, lex_pos_ty *msgid_pos, char *msgid_plural,
		     char *msgstr, size_t msgstr_len, lex_pos_ty *msgstr_pos,
		     char *prev_msgctxt,
		     char *prev_msgid, char *prev_msgid_plural,
		     bool obsolete)
{
  /* Test for header entry.  Ignore fuzziness of the header entry.  */
  if (msgctxt == NULL && msgid[0] == '\0' && !obsolete)
    po_lex_charset_set (msgstr, gram_pos.file_name);

  po_callback_message (msgctxt,
		       msgid, msgid_pos, msgid_plural,
		       msgstr, msgstr_len, msgstr_pos,
		       prev_msgctxt, prev_msgid, prev_msgid_plural,
		       false, obsolete);
}

#define free_message_intro(value) \
  if ((value).prev_ctxt != NULL)	\
    free ((value).prev_ctxt);		\
  if ((value).prev_id != NULL)		\
    free ((value).prev_id);		\
  if ((value).prev_id_plural != NULL)	\
    free ((value).prev_id_plural);	\
  if ((value).ctxt != NULL)		\
    free ((value).ctxt);


# line 143 "po-gram-gen.y"
typedef union 
{
  struct { char *string; lex_pos_ty pos; bool obsolete; } string;
  struct { string_list_ty stringlist; lex_pos_ty pos; bool obsolete; } stringlist;
  struct { long number; lex_pos_ty pos; bool obsolete; } number;
  struct { lex_pos_ty pos; bool obsolete; } pos;
  struct { char *ctxt; char *id; char *id_plural; lex_pos_ty pos; bool obsolete; } prev;
  struct { char *prev_ctxt; char *prev_id; char *prev_id_plural; char *ctxt; lex_pos_ty pos; bool obsolete; } message_intro;
  struct { struct msgstr_def rhs; lex_pos_ty pos; bool obsolete; } rhs;
} YYSTYPE;
#ifdef __cplusplus
#  include <stdio.h>
#  include <yacc.h>
#endif	/* __cplusplus */ 
# define COMMENT 257
# define DOMAIN 258
# define JUNK 259
# define PREV_MSGCTXT 260
# define PREV_MSGID 261
# define PREV_MSGID_PLURAL 262
# define PREV_STRING 263
# define MSGCTXT 264
# define MSGID 265
# define MSGID_PLURAL 266
# define MSGSTR 267
# define NAME 268
# define NUMBER 269
# define STRING 270
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif

/* __YYSCLASS defines the scoping/storage class for global objects
 * that are NOT renamed by the -p option.  By default these names
 * are going to be 'static' so that multi-definition errors
 * will not occur with multiple parsers.
 * If you want (unsupported) access to internal names you need
 * to define this to be null so it implies 'extern' scope.
 * This should not be used in conjunction with -p.
 */
#ifndef __YYSCLASS
# define __YYSCLASS static
#endif
YYSTYPE yylval;
__YYSCLASS YYSTYPE yyval;
typedef int yytabelem;
# define YYERRCODE 256
__YYSCLASS yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 30
# define YYLAST 182
__YYSCLASS yytabelem yyact[]={

    28,    24,    36,    30,    27,     5,     6,     7,    27,    15,
    14,    27,    18,    12,    11,    16,    42,    38,    12,    11,
    34,    32,    32,    33,    32,    22,    43,    36,    17,    29,
    21,     9,    26,     4,     3,     2,     1,     8,    10,    31,
    25,    20,    19,    13,     0,     0,    23,     0,     0,     0,
     0,     0,     0,    35,     0,     0,    39,    40,    37,     0,
     0,     0,     0,     0,    41,     0,     0,    39,     0,     0,
     0,     0,    44,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    18 };
__YYSCLASS yytabelem yypact[]={

 -3400,  -251, -3400, -3400, -3400, -3400, -3400,  -255,  -258, -3400,
  -246, -3400,  -258,  -238, -3400,  -238, -3400,  -266, -3400, -3400,
  -262,  -239, -3400,  -241,   -89,  -250,  -250, -3400,  -258, -3400,
 -3400, -3400, -3400,  -238, -3400,  -259,  -253,  -250,   -64, -3400,
  -259,  -242,   -67,  -258,  -259 };
__YYSCLASS yytabelem yypgo[]={

     0,    31,    43,    40,    39,    28,    30,    38,    37,    29,
    32,    36,    35,    34,    33 };
__YYSCLASS yytabelem yyr1[]={

     0,    11,    11,    11,    11,    11,    12,    13,    14,    14,
    14,    14,    14,     8,     8,     7,     7,     1,     1,     2,
     2,     3,     4,    10,    10,     9,     5,     5,     6,     6 };
__YYSCLASS yytabelem yyr2[]={

     0,     0,     4,     4,     4,     4,     3,     5,     9,     9,
     7,     7,     5,     3,     5,     5,     7,     3,     7,     3,
     7,     5,     5,     3,     5,    11,     3,     5,     3,     5 };
__YYSCLASS yytabelem yychk[]={

 -3400,   -11,   -12,   -13,   -14,   256,   257,   258,    -8,    -1,
    -7,   265,   264,    -2,   261,   260,   270,    -5,   270,    -1,
    -5,    -6,   263,    -6,   267,    -3,   -10,   270,   266,    -9,
   265,    -4,   263,   262,   261,    -5,    91,   -10,   267,    -9,
    -5,    -6,   269,    93,    -5 };
__YYSCLASS yytabelem yydef[]={

     1,    -2,     2,     3,     4,     5,     6,     0,     0,    13,
     0,    17,     0,     0,    19,     0,     7,    12,    26,    14,
     0,    15,    28,     0,     0,    10,    11,    27,     0,    23,
    18,    16,    29,     0,    20,     8,     0,     9,     0,    24,
    21,    22,     0,     0,    25 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

__YYSCLASS yytoktype yytoks[] =
{
	"COMMENT",	257,
	"DOMAIN",	258,
	"JUNK",	259,
	"PREV_MSGCTXT",	260,
	"PREV_MSGID",	261,
	"PREV_MSGID_PLURAL",	262,
	"PREV_STRING",	263,
	"MSGCTXT",	264,
	"MSGID",	265,
	"MSGID_PLURAL",	266,
	"MSGSTR",	267,
	"NAME",	268,
	"[",	91,
	"]",	93,
	"NUMBER",	269,
	"STRING",	270,
	"-unknown-",	-1	/* ends search */
};

__YYSCLASS char * yyreds[] =
{
	"-no such reduction-",
	"po_file : /* empty */",
	"po_file : po_file comment",
	"po_file : po_file domain",
	"po_file : po_file message",
	"po_file : po_file error",
	"comment : COMMENT",
	"domain : DOMAIN STRING",
	"message : message_intro string_list MSGSTR string_list",
	"message : message_intro string_list msgid_pluralform pluralform_list",
	"message : message_intro string_list msgid_pluralform",
	"message : message_intro string_list pluralform_list",
	"message : message_intro string_list",
	"message_intro : msg_intro",
	"message_intro : prev msg_intro",
	"prev : prev_msg_intro prev_string_list",
	"prev : prev_msg_intro prev_string_list prev_msgid_pluralform",
	"msg_intro : MSGID",
	"msg_intro : MSGCTXT string_list MSGID",
	"prev_msg_intro : PREV_MSGID",
	"prev_msg_intro : PREV_MSGCTXT prev_string_list PREV_MSGID",
	"msgid_pluralform : MSGID_PLURAL string_list",
	"prev_msgid_pluralform : PREV_MSGID_PLURAL prev_string_list",
	"pluralform_list : pluralform",
	"pluralform_list : pluralform_list pluralform",
	"pluralform : MSGSTR '[' NUMBER ']' string_list",
	"string_list : STRING",
	"string_list : string_list STRING",
	"prev_string_list : PREV_STRING",
	"prev_string_list : prev_string_list PREV_STRING",
};
#endif /* YYDEBUG */
#define YYFLAG  (-3400)
/* @(#) $Revision: 70.7 $ */    

/*
** Skeleton parser driver for yacc output
*/

#if defined(NLS) && !defined(NL_SETN)
#include <msgbuf.h>
#endif

#ifndef nl_msg
#define nl_msg(i,s) (s)
#endif

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab

#ifndef __RUNTIME_YYMAXDEPTH
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#else
#define YYACCEPT	{free_stacks(); return(0);}
#define YYABORT		{free_stacks(); return(1);}
#endif

#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( (nl_msg(30001,"syntax error - cannot backup")) );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/

#ifdef YYREENTRANT
__thread int yydebug;		/* set to 1 to get debugging */
#else
int yydebug;			/* set to 1 to get debugging */
#endif

/*
** driver internal defines
*/
/* define for YYFLAG now generated by yacc program. */
/*#define YYFLAG		(FLAGVAL)*/

/*
** global variables used by the parser
*/
# ifndef __RUNTIME_YYMAXDEPTH

#   ifdef YYREENTRANT
__thread __YYSCLASS YYSTYPE yyv[ YYMAXDEPTH ];  /* value stack */
__thread __YYSCLASS int yys[ YYMAXDEPTH ];      /* state stack */
#   else
__YYSCLASS YYSTYPE yyv[ YYMAXDEPTH ];           /* value stack */
__YYSCLASS int yys[ YYMAXDEPTH ];               /* state stack */
#   endif

# else

#  ifdef YYREENTRANT
__thread __YYSCLASS YYSTYPE *yyv;               /* pointer to malloc'ed stack st
value stack */
__thread __YYSCLASS int *yys;                   /* pointer to malloc'ed stack st
ack */
#  else
__YYSCLASS YYSTYPE *yyv;                        /* pointer to malloc'ed value st
ack */
__YYSCLASS int *yys;                            /* pointer to malloc'ed stack stack */
#  endif

#if defined(__STDC__) || defined (__cplusplus)
#include <stdlib.h>
#else
	extern char *malloc();
	extern char *realloc();
	extern void free();
#endif /* __STDC__ or __cplusplus */


static int allocate_stacks(); 
static void free_stacks();
# ifndef YYINCREMENT
# define YYINCREMENT (YYMAXDEPTH/2) + 10
# endif
# endif	/* __RUNTIME_YYMAXDEPTH */
long  yymaxdepth = YYMAXDEPTH;


#ifdef YYREENTRANT

__thread __YYSCLASS YYSTYPE *yypv;      /* top of value stack */
__thread __YYSCLASS int *yyps;          /* top of state stack */

__thread __YYSCLASS int yystate;        /* current state */
__thread __YYSCLASS int yytmp;          /* extra var (lasts between blocks) */

__thread int yynerrs;                   /* number of errors */
__thread __YYSCLASS int yyerrflag;      /* error recovery flag */
__thread int yychar;                    /* current input token number */

__thread extern int yyinit_key;           /* init TLS data once */
extern int TLS_INIT; 

#else

__YYSCLASS YYSTYPE *yypv;               /* top of value stack */
__YYSCLASS int *yyps;                   /* top of state stack */

__YYSCLASS int yystate;                 /* current state */
__YYSCLASS int yytmp;                   /* extra var (lasts between blocks) */

int yynerrs;                            /* number of errors */
__YYSCLASS int yyerrflag;               /* error recovery flag */
int yychar;
#endif


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
l*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */


        /*
        ** Initialize externals - yyparse may be called more than once
        */

#ifdef YYREENTRANT
        if (yyinit_key != TLS_INIT)
        {
            yyinit_key = TLS_INIT; 
            yyinit_tls();
        }
#endif

	/*
	** Initialize externals - yyparse may be called more than once
	*/
# ifdef __RUNTIME_YYMAXDEPTH
	if (allocate_stacks()) YYABORT;
# endif
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
# ifndef __RUNTIME_YYMAXDEPTH
			yyerror( (nl_msg(30002,"yacc stack overflow")) );
			YYABORT;
# else
			/* save old stack bases to recalculate pointers */
			YYSTYPE * yyv_old = yyv;
			int * yys_old = yys;
			yymaxdepth += YYINCREMENT;
			yys = (int *) realloc(yys, yymaxdepth * sizeof(int));
			yyv = (YYSTYPE *) realloc(yyv, yymaxdepth * sizeof(YYSTYPE));
			if (yys==0 || yyv==0) {
			    yyerror( (nl_msg(30002,"yacc stack overflow")) );
			    YYABORT;
			    }
			/* Reset pointers into stack */
			yy_ps = (yy_ps - yys_old) + yys;
			yyps = (yyps - yys_old) + yys;
			yy_pv = (yy_pv - yyv_old) + yyv;
			yypv = (yypv - yyv_old) + yyv;
# endif

		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( (nl_msg(30003,"syntax error")) );
				yynerrs++;
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 6:
# line 180 "po-gram-gen.y"
{
		  po_callback_comment_dispatcher (yypvt[-0].string.string);
		} break;
case 7:
# line 188 "po-gram-gen.y"
{
		   po_callback_domain (yypvt[-0].string.string);
		} break;
case 8:
# line 196 "po-gram-gen.y"
{
		  char *string2 = string_list_concat_destroy (&yypvt[-2].stringlist.stringlist);
		  char *string4 = string_list_concat_destroy (&yypvt[-0].stringlist.stringlist);

		  check_obsolete (yypvt[-3].message_intro, yypvt[-2].stringlist);
		  check_obsolete (yypvt[-3].message_intro, yypvt[-1].pos);
		  check_obsolete (yypvt[-3].message_intro, yypvt[-0].stringlist);
		  if (!yypvt[-3].message_intro.obsolete || pass_obsolete_entries)
		    do_callback_message (yypvt[-3].message_intro.ctxt, string2, &yypvt[-3].message_intro.pos, NULL,
					 string4, strlen (string4) + 1, &yypvt[-1].pos.pos,
					 yypvt[-3].message_intro.prev_ctxt,
					 yypvt[-3].message_intro.prev_id, yypvt[-3].message_intro.prev_id_plural,
					 yypvt[-3].message_intro.obsolete);
		  else
		    {
		      free_message_intro (yypvt[-3].message_intro);
		      free (string2);
		      free (string4);
		    }
		} break;
case 9:
# line 217 "po-gram-gen.y"
{
		  char *string2 = string_list_concat_destroy (&yypvt[-2].stringlist.stringlist);

		  check_obsolete (yypvt[-3].message_intro, yypvt[-2].stringlist);
		  check_obsolete (yypvt[-3].message_intro, yypvt[-1].string);
		  check_obsolete (yypvt[-3].message_intro, yypvt[-0].rhs);
		  if (!yypvt[-3].message_intro.obsolete || pass_obsolete_entries)
		    do_callback_message (yypvt[-3].message_intro.ctxt, string2, &yypvt[-3].message_intro.pos, yypvt[-1].string.string,
					 yypvt[-0].rhs.rhs.msgstr, yypvt[-0].rhs.rhs.msgstr_len, &yypvt[-0].rhs.pos,
					 yypvt[-3].message_intro.prev_ctxt,
					 yypvt[-3].message_intro.prev_id, yypvt[-3].message_intro.prev_id_plural,
					 yypvt[-3].message_intro.obsolete);
		  else
		    {
		      free_message_intro (yypvt[-3].message_intro);
		      free (string2);
		      free (yypvt[-1].string.string);
		      free (yypvt[-0].rhs.rhs.msgstr);
		    }
		} break;
case 10:
# line 238 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-2].message_intro, yypvt[-1].stringlist);
		  check_obsolete (yypvt[-2].message_intro, yypvt[-0].string);
		  po_gram_error_at_line (&yypvt[-2].message_intro.pos, _("missing `msgstr[]' section"));
		  free_message_intro (yypvt[-2].message_intro);
		  string_list_destroy (&yypvt[-1].stringlist.stringlist);
		  free (yypvt[-0].string.string);
		} break;
case 11:
# line 247 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-2].message_intro, yypvt[-1].stringlist);
		  check_obsolete (yypvt[-2].message_intro, yypvt[-0].rhs);
		  po_gram_error_at_line (&yypvt[-2].message_intro.pos, _("missing `msgid_plural' section"));
		  free_message_intro (yypvt[-2].message_intro);
		  string_list_destroy (&yypvt[-1].stringlist.stringlist);
		  free (yypvt[-0].rhs.rhs.msgstr);
		} break;
case 12:
# line 256 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-1].message_intro, yypvt[-0].stringlist);
		  po_gram_error_at_line (&yypvt[-1].message_intro.pos, _("missing `msgstr' section"));
		  free_message_intro (yypvt[-1].message_intro);
		  string_list_destroy (&yypvt[-0].stringlist.stringlist);
		} break;
case 13:
# line 267 "po-gram-gen.y"
{
		  yyval.message_intro.prev_ctxt = NULL;
		  yyval.message_intro.prev_id = NULL;
		  yyval.message_intro.prev_id_plural = NULL;
		  yyval.message_intro.ctxt = yypvt[-0].string.string;
		  yyval.message_intro.pos = yypvt[-0].string.pos;
		  yyval.message_intro.obsolete = yypvt[-0].string.obsolete;
		} break;
case 14:
# line 276 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-1].prev, yypvt[-0].string);
		  yyval.message_intro.prev_ctxt = yypvt[-1].prev.ctxt;
		  yyval.message_intro.prev_id = yypvt[-1].prev.id;
		  yyval.message_intro.prev_id_plural = yypvt[-1].prev.id_plural;
		  yyval.message_intro.ctxt = yypvt[-0].string.string;
		  yyval.message_intro.pos = yypvt[-0].string.pos;
		  yyval.message_intro.obsolete = yypvt[-0].string.obsolete;
		} break;
case 15:
# line 290 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-1].string, yypvt[-0].stringlist);
		  yyval.prev.ctxt = yypvt[-1].string.string;
		  yyval.prev.id = string_list_concat_destroy (&yypvt[-0].stringlist.stringlist);
		  yyval.prev.id_plural = NULL;
		  yyval.prev.pos = yypvt[-1].string.pos;
		  yyval.prev.obsolete = yypvt[-1].string.obsolete;
		} break;
case 16:
# line 299 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-2].string, yypvt[-1].stringlist);
		  check_obsolete (yypvt[-2].string, yypvt[-0].string);
		  yyval.prev.ctxt = yypvt[-2].string.string;
		  yyval.prev.id = string_list_concat_destroy (&yypvt[-1].stringlist.stringlist);
		  yyval.prev.id_plural = yypvt[-0].string.string;
		  yyval.prev.pos = yypvt[-2].string.pos;
		  yyval.prev.obsolete = yypvt[-2].string.obsolete;
		} break;
case 17:
# line 313 "po-gram-gen.y"
{
		  yyval.string.string = NULL;
		  yyval.string.pos = yypvt[-0].pos.pos;
		  yyval.string.obsolete = yypvt[-0].pos.obsolete;
		} break;
case 18:
# line 319 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-2].pos, yypvt[-1].stringlist);
		  check_obsolete (yypvt[-2].pos, yypvt[-0].pos);
		  yyval.string.string = string_list_concat_destroy (&yypvt[-1].stringlist.stringlist);
		  yyval.string.pos = yypvt[-0].pos.pos;
		  yyval.string.obsolete = yypvt[-0].pos.obsolete;
		} break;
case 19:
# line 330 "po-gram-gen.y"
{
		  yyval.string.string = NULL;
		  yyval.string.pos = yypvt[-0].pos.pos;
		  yyval.string.obsolete = yypvt[-0].pos.obsolete;
		} break;
case 20:
# line 336 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-2].pos, yypvt[-1].stringlist);
		  check_obsolete (yypvt[-2].pos, yypvt[-0].pos);
		  yyval.string.string = string_list_concat_destroy (&yypvt[-1].stringlist.stringlist);
		  yyval.string.pos = yypvt[-0].pos.pos;
		  yyval.string.obsolete = yypvt[-0].pos.obsolete;
		} break;
case 21:
# line 348 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-1].pos, yypvt[-0].stringlist);
		  plural_counter = 0;
		  yyval.string.string = string_list_concat_destroy (&yypvt[-0].stringlist.stringlist);
		  yyval.string.pos = yypvt[-1].pos.pos;
		  yyval.string.obsolete = yypvt[-1].pos.obsolete;
		} break;
case 22:
# line 359 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-1].pos, yypvt[-0].stringlist);
		  yyval.string.string = string_list_concat_destroy (&yypvt[-0].stringlist.stringlist);
		  yyval.string.pos = yypvt[-1].pos.pos;
		  yyval.string.obsolete = yypvt[-1].pos.obsolete;
		} break;
case 23:
# line 370 "po-gram-gen.y"
{
		  yyval.rhs = yypvt[-0].rhs;
		} break;
case 24:
# line 374 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-1].rhs, yypvt[-0].rhs);
		  yyval.rhs.rhs.msgstr = (char *) xmalloc (yypvt[-1].rhs.rhs.msgstr_len + yypvt[-0].rhs.rhs.msgstr_len);
		  memcpy (yyval.rhs.rhs.msgstr, yypvt[-1].rhs.rhs.msgstr, yypvt[-1].rhs.rhs.msgstr_len);
		  memcpy (yyval.rhs.rhs.msgstr + yypvt[-1].rhs.rhs.msgstr_len, yypvt[-0].rhs.rhs.msgstr, yypvt[-0].rhs.rhs.msgstr_len);
		  yyval.rhs.rhs.msgstr_len = yypvt[-1].rhs.rhs.msgstr_len + yypvt[-0].rhs.rhs.msgstr_len;
		  free (yypvt[-1].rhs.rhs.msgstr);
		  free (yypvt[-0].rhs.rhs.msgstr);
		  yyval.rhs.pos = yypvt[-1].rhs.pos;
		  yyval.rhs.obsolete = yypvt[-1].rhs.obsolete;
		} break;
case 25:
# line 389 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-4].pos, yypvt[-3].pos);
		  check_obsolete (yypvt[-4].pos, yypvt[-2].number);
		  check_obsolete (yypvt[-4].pos, yypvt[-1].pos);
		  check_obsolete (yypvt[-4].pos, yypvt[-0].stringlist);
		  if (yypvt[-2].number.number != plural_counter)
		    {
		      if (plural_counter == 0)
			po_gram_error_at_line (&yypvt[-4].pos.pos, _("first plural form has nonzero index"));
		      else
			po_gram_error_at_line (&yypvt[-4].pos.pos, _("plural form has wrong index"));
		    }
		  plural_counter++;
		  yyval.rhs.rhs.msgstr = string_list_concat_destroy (&yypvt[-0].stringlist.stringlist);
		  yyval.rhs.rhs.msgstr_len = strlen (yyval.rhs.rhs.msgstr) + 1;
		  yyval.rhs.pos = yypvt[-4].pos.pos;
		  yyval.rhs.obsolete = yypvt[-4].pos.obsolete;
		} break;
case 26:
# line 412 "po-gram-gen.y"
{
		  string_list_init (&yyval.stringlist.stringlist);
		  string_list_append (&yyval.stringlist.stringlist, yypvt[-0].string.string);
		  yyval.stringlist.pos = yypvt[-0].string.pos;
		  yyval.stringlist.obsolete = yypvt[-0].string.obsolete;
		} break;
case 27:
# line 419 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-1].stringlist, yypvt[-0].string);
		  yyval.stringlist.stringlist = yypvt[-1].stringlist.stringlist;
		  string_list_append (&yyval.stringlist.stringlist, yypvt[-0].string.string);
		  yyval.stringlist.pos = yypvt[-1].stringlist.pos;
		  yyval.stringlist.obsolete = yypvt[-1].stringlist.obsolete;
		} break;
case 28:
# line 430 "po-gram-gen.y"
{
		  string_list_init (&yyval.stringlist.stringlist);
		  string_list_append (&yyval.stringlist.stringlist, yypvt[-0].string.string);
		  yyval.stringlist.pos = yypvt[-0].string.pos;
		  yyval.stringlist.obsolete = yypvt[-0].string.obsolete;
		} break;
case 29:
# line 437 "po-gram-gen.y"
{
		  check_obsolete (yypvt[-1].stringlist, yypvt[-0].string);
		  yyval.stringlist.stringlist = yypvt[-1].stringlist.stringlist;
		  string_list_append (&yyval.stringlist.stringlist, yypvt[-0].string.string);
		  yyval.stringlist.pos = yypvt[-1].stringlist.pos;
		  yyval.stringlist.obsolete = yypvt[-1].stringlist.obsolete;
		} break;
	}
	goto yystack;		/* reset registers in driver code */
}

# ifdef __RUNTIME_YYMAXDEPTH

static int allocate_stacks() {
	/* allocate the yys and yyv stacks */
	yys = (int *) malloc(yymaxdepth * sizeof(int));
	yyv = (YYSTYPE *) malloc(yymaxdepth * sizeof(YYSTYPE));

	if (yys==0 || yyv==0) {
	   yyerror( (nl_msg(30004,"unable to allocate space for yacc stacks")) );
	   return(1);
	   }
	else return(0);

}


static void free_stacks() {
	if (yys!=0) free((char *) yys);
	if (yyv!=0) free((char *) yyv);
}

# endif  /* defined(__RUNTIME_YYMAXDEPTH) */

