/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse rapicparse
#define yylex   rapiclex
#define yyerror rapicerror
#define yylval  rapiclval
#define yychar  rapicchar
#define yydebug rapicdebug
#define yynerrs rapicnerrs


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     IMAGE = 258,
     IMAGESCANS = 259,
     IMAGESIZE = 260,
     IMAGEEND = 261,
     SCAN = 262,
     IMAGEHEADEREND = 263,
     NUMBER = 264,
     ALPHA = 265,
     FLOATNUMBER = 266,
     BRACKETNUM = 267,
     COUNTRY = 268,
     NAME = 269,
     STNID = 270,
     LATITUDE = 271,
     LONGITUDE = 272,
     HEIGHT = 273,
     DATE = 274,
     TIME = 275,
     TIMESTAMP = 276,
     VERS = 277,
     FREQUENCY = 278,
     PRF = 279,
     PULSELENGTH = 280,
     RNGRES = 281,
     ANGRES = 282,
     ANGLERATE = 283,
     CLEARAIR = 284,
     ON = 285,
     OFF = 286,
     VIDRES = 287,
     STARTRNG = 288,
     ENDRNG = 289,
     PRODUCT = 290,
     PASS = 291,
     IMGFMT = 292,
     ELEV = 293,
     VIDEO = 294,
     VELLVL = 295,
     NYQUIST = 296,
     UNFOLDING = 297,
     AT = 298,
     VOLUMETRIC = 299,
     NORMAL = 300,
     OF = 301,
     REFL = 302,
     VEL = 303,
     UNCORREFL = 304,
     ZDR = 305,
     WID = 306,
     NONE = 307,
     RAYDATA = 308,
     ENDRADARIMAGE = 309
   };
#endif
/* Tokens.  */
#define IMAGE 258
#define IMAGESCANS 259
#define IMAGESIZE 260
#define IMAGEEND 261
#define SCAN 262
#define IMAGEHEADEREND 263
#define NUMBER 264
#define ALPHA 265
#define FLOATNUMBER 266
#define BRACKETNUM 267
#define COUNTRY 268
#define NAME 269
#define STNID 270
#define LATITUDE 271
#define LONGITUDE 272
#define HEIGHT 273
#define DATE 274
#define TIME 275
#define TIMESTAMP 276
#define VERS 277
#define FREQUENCY 278
#define PRF 279
#define PULSELENGTH 280
#define RNGRES 281
#define ANGRES 282
#define ANGLERATE 283
#define CLEARAIR 284
#define ON 285
#define OFF 286
#define VIDRES 287
#define STARTRNG 288
#define ENDRNG 289
#define PRODUCT 290
#define PASS 291
#define IMGFMT 292
#define ELEV 293
#define VIDEO 294
#define VELLVL 295
#define NYQUIST 296
#define UNFOLDING 297
#define AT 298
#define VOLUMETRIC 299
#define NORMAL 300
#define OF 301
#define REFL 302
#define VEL 303
#define UNCORREFL 304
#define ZDR 305
#define WID 306
#define NONE 307
#define RAYDATA 308
#define ENDRADARIMAGE 309




/* Copy the first part of user declarations.  */
#line 23 "rapic.y"

#define USE_RSL_VARS
#include "rapic_routines.h"
#include "rsl.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>

int rapicerror(char *s);
int rapicwrap(char *s);
int rapicwrap(char *s);
int yywrap(char *s);
int rapiclex(void);

int nsweep = 0;
float angres;
Radar *radar, *rapic_radar = NULL;
Volume *volume;
Sweep *sweep;
Ray *ray;

/* Rapic format declarations. */
Rapic_sweep_header rh;
Rapic_sweep *rs;

unsigned char outbuf[2000000];
int outbytes;
float azim, elev;
float save_elev;
int delta_time;
int nray = 0;
int ifield;
int ivolume, isweep, iray;
int station_id;

int sweepcount[5];

extern int radar_verbose_flag;
float rapic_nyquist;
  

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 326 "rapic.y"
{
  Charlen token;
}
/* Line 193 of yacc.c.  */
#line 257 "rapic.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 270 "rapic.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  17
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   194

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  56
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  53
/* YYNRULES -- Number of rules.  */
#define YYNRULES  100
/* YYNRULES -- Number of states.  */
#define YYNSTATES  152

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   309

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    55,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     7,     9,    12,    16,    18,    22,    25,
      27,    30,    31,    35,    38,    41,    43,    55,    57,    60,
      61,    63,    65,    68,    69,    72,    75,    78,    81,    84,
      87,    90,    93,    96,    99,   102,   105,   108,   111,   114,
     117,   120,   123,   126,   129,   133,   135,   138,   141,   144,
     147,   150,   153,   156,   158,   160,   162,   164,   166,   168,
     170,   172,   174,   176,   178,   180,   182,   184,   186,   188,
     190,   192,   194,   196,   198,   200,   202,   204,   206,   208,
     210,   212,   214,   216,   218,   220,   222,   224,   226,   230,
     232,   234,   236,   238,   240,   242,   244,   246,   248,   250,
     252
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      57,     0,    -1,    62,    58,    61,    -1,    59,    -1,    58,
      59,    -1,    60,    66,    54,    -1,    68,    -1,     6,    72,
      74,    -1,    63,     8,    -1,    64,    -1,    63,    64,    -1,
      -1,     3,    72,    74,    -1,     4,    71,    -1,     5,    71,
      -1,    65,    -1,     7,    73,    55,    72,    75,    76,    77,
      78,    76,    79,    80,    -1,    67,    -1,    66,    67,    -1,
      -1,    53,    -1,    69,    -1,    68,    69,    -1,    -1,    14,
      83,    -1,    13,    82,    -1,    15,    84,    -1,    16,    85,
      -1,    17,    86,    -1,    18,    87,    -1,    19,    81,    -1,
      20,    88,    -1,    21,    89,    -1,    22,    90,    -1,    23,
      91,    -1,    24,    92,    -1,    25,    93,    -1,    26,    94,
      -1,    28,    96,    -1,    29,    97,    -1,    27,    95,    -1,
      32,    98,    -1,    33,    99,    -1,    34,    99,    -1,    35,
     100,    12,    -1,    35,    -1,    36,   101,    -1,    38,    77,
      -1,    40,   106,    -1,    41,   107,    -1,    39,   105,    -1,
      37,   104,    -1,    42,   108,    -1,    71,    -1,    11,    -1,
       9,    -1,    71,    -1,    71,    -1,    71,    -1,    71,    -1,
      70,    -1,    10,    -1,    70,    -1,    71,    -1,    71,    -1,
      71,    -1,    71,    -1,    71,    -1,    10,    -1,    71,    -1,
      70,    -1,    70,    -1,    70,    -1,    70,    -1,    71,    -1,
      70,    -1,    71,    -1,    71,    -1,    70,    -1,    71,    -1,
      70,    -1,    70,    -1,    30,    -1,    31,    -1,    71,    -1,
      71,    -1,    44,    -1,    45,    -1,   102,    46,   103,    -1,
      71,    -1,    71,    -1,    10,    -1,    47,    -1,    48,    -1,
      49,    -1,    50,    -1,    51,    -1,    70,    -1,    70,    -1,
      52,    -1,    71,    55,    71,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   334,   334,   346,   347,   349,   359,   372,   374,   413,
     414,   415,   418,   429,   430,   437,   440,   450,   451,   452,
     455,   507,   508,   509,   515,   516,   517,   518,   519,   520,
     521,   522,   523,   524,   525,   526,   527,   528,   529,   530,
     531,   532,   533,   534,   535,   536,   537,   538,   539,   540,
     545,   546,   547,   550,   551,   553,   555,   556,   557,   558,
     559,   560,   561,   562,   563,   564,   565,   567,   568,   569,
     570,   571,   572,   573,   575,   576,   578,   579,   580,   581,
     582,   583,   584,   585,   586,   587,   588,   589,   591,   592,
     593,   594,   596,   597,   598,   599,   600,   602,   603,   605,
     606
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IMAGE", "IMAGESCANS", "IMAGESIZE",
  "IMAGEEND", "SCAN", "IMAGEHEADEREND", "NUMBER", "ALPHA", "FLOATNUMBER",
  "BRACKETNUM", "COUNTRY", "NAME", "STNID", "LATITUDE", "LONGITUDE",
  "HEIGHT", "DATE", "TIME", "TIMESTAMP", "VERS", "FREQUENCY", "PRF",
  "PULSELENGTH", "RNGRES", "ANGRES", "ANGLERATE", "CLEARAIR", "ON", "OFF",
  "VIDRES", "STARTRNG", "ENDRNG", "PRODUCT", "PASS", "IMGFMT", "ELEV",
  "VIDEO", "VELLVL", "NYQUIST", "UNFOLDING", "AT", "VOLUMETRIC", "NORMAL",
  "OF", "REFL", "VEL", "UNCORREFL", "ZDR", "WID", "NONE", "RAYDATA",
  "ENDRADARIMAGE", "':'", "$accept", "rapic_recognized", "sweeps", "sweep",
  "sweepheader", "imageend", "complete_header", "imageheader",
  "imageheader_item", "scanlist", "rays", "ray", "scanheader",
  "scanheaditem", "real", "number", "seqno", "scanno", "imgno", "datetime",
  "dc", "elev", "fieldno", "offset", "size", "datno", "code", "namestr",
  "idno", "lat", "lon", "alt", "hhmm", "yyyymoddhhmmss", "versionNumber",
  "freq", "prf", "len", "gatewidth", "angle", "anglerate", "clearair",
  "res", "rng", "typeid", "noofnscans", "no", "nscans", "type", "field",
  "level", "nyq", "ratio", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    56,    57,    58,    58,    59,    60,    61,    62,    63,
      63,    63,    64,    64,    64,    64,    65,    66,    66,    66,
      67,    68,    68,    68,    69,    69,    69,    69,    69,    69,
      69,    69,    69,    69,    69,    69,    69,    69,    69,    69,
      69,    69,    69,    69,    69,    69,    69,    69,    69,    69,
      69,    69,    69,    70,    70,    71,    72,    73,    74,    75,
      76,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    97,    98,    99,   100,   100,   101,   102,
     103,   104,   105,   105,   105,   105,   105,   106,   107,   108,
     108
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     3,     1,     2,     3,     1,     3,     2,     1,
       2,     0,     3,     2,     2,     1,    11,     1,     2,     0,
       1,     1,     2,     0,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     3,     1,     2,     2,     2,     2,
       2,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
      11,     0,     0,     0,     0,     0,    23,     0,     9,    15,
      55,    56,     0,    13,    14,    57,     0,     1,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    45,     0,
       0,     0,     0,     0,     0,     0,    23,     3,    19,     6,
      21,     8,    10,    58,    12,     0,    67,    25,    68,    24,
      69,    26,    54,    70,    53,    27,    71,    28,    72,    29,
      66,    30,    73,    31,    74,    32,    75,    33,    76,    34,
      77,    35,    78,    36,    79,    37,    80,    40,    81,    38,
      82,    83,    39,    84,    41,    85,    42,    43,    86,    87,
       0,    89,    46,     0,    91,    51,    62,    47,    92,    93,
      94,    95,    96,    50,    97,    48,    98,    49,    99,     0,
      52,     0,     4,     2,    20,     0,    17,    22,     0,    44,
       0,     0,     0,     5,    18,    59,     0,    90,    88,   100,
       7,    61,    60,     0,     0,    63,     0,     0,    64,     0,
      65,    16
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     5,    46,    47,    48,   123,     6,     7,     8,     9,
     125,   126,    49,    50,   106,    64,    12,    16,    54,   136,
     143,   107,   146,   149,   151,    71,    57,    59,    61,    65,
      67,    69,    73,    75,    77,    79,    81,    83,    85,    87,
      89,    92,    94,    96,   100,   102,   103,   138,   105,   113,
     115,   117,   120
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -65
static const yytype_int16 yypact[] =
{
      11,    13,    13,    13,    13,    24,   152,     5,   -65,   -65,
     -65,   -65,    13,   -65,   -65,   -65,   -29,   -65,    13,    19,
      13,    -4,    -4,    -4,    13,    -4,    13,    -4,    13,    13,
      -4,    13,    -4,    -4,   -10,    13,    13,    13,     2,    13,
      27,    -4,     8,    -4,    -4,    -3,    58,   -65,   -11,   152,
     -65,   -65,   -65,   -65,   -65,    13,   -65,   -65,   -65,   -65,
     -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,
     -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,
     -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,
     -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,
      40,   -65,   -65,     7,   -65,   -65,   -65,   -65,   -65,   -65,
     -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,    14,
     -65,    13,   -65,   -65,   -65,    12,   -65,   -65,    13,   -65,
      13,    13,    13,   -65,   -65,   -65,    22,   -65,   -65,   -65,
     -65,   -65,   -65,    -4,    13,   -65,    22,    13,   -65,    13,
     -65,   -65
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -65,   -65,   -65,    17,   -65,   -65,   -65,   -65,    53,   -65,
     -65,   -58,   -65,    39,    18,    -1,   -51,   -65,   -64,   -65,
     -57,   -42,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,
     -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,
     -65,   -65,   -65,    65,   -65,   -65,   -65,   -65,   -65,   -65,
     -65,   -65,   -65
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      11,    13,    14,    15,   128,    10,    10,    62,     1,     2,
       3,    53,     4,    51,     1,     2,     3,    56,     4,    60,
      90,    91,    10,    70,    17,    74,    55,    78,    80,    58,
      84,    10,   141,    62,    93,    95,    95,   104,   101,    63,
      66,    68,   124,    72,   119,    76,    98,    99,    82,   118,
      86,    88,   129,   130,    11,   108,   109,   110,   111,   112,
      52,   114,   116,   122,   121,   124,   133,   134,   140,   131,
     132,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,   127,   147,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,   144,    97,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      11,     0,     0,     0,     0,     0,     0,   135,     0,   137,
     139,    53,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   145,     0,     0,   148,     0,   150,     0,
       0,     0,     0,     0,   142,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   142,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,     0,     0,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45
};

static const yytype_int16 yycheck[] =
{
       1,     2,     3,     4,    55,     9,     9,    11,     3,     4,
       5,    12,     7,     8,     3,     4,     5,    18,     7,    20,
      30,    31,     9,    24,     0,    26,    55,    28,    29,    10,
      31,     9,    10,    11,    35,    36,    37,    10,    39,    21,
      22,    23,    53,    25,    45,    27,    44,    45,    30,    52,
      32,    33,    12,    46,    55,    47,    48,    49,    50,    51,
       7,    43,    44,    46,     6,    53,    54,   125,   132,    55,
     121,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    49,   146,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,   143,    37,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     121,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,   130,
     131,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   144,    -1,    -1,   147,    -1,   149,    -1,
      -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   146,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    -1,    -1,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     7,    57,    62,    63,    64,    65,
       9,    71,    72,    71,    71,    71,    73,     0,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    58,    59,    60,    68,
      69,     8,    64,    71,    74,    55,    71,    82,    10,    83,
      71,    84,    11,    70,    71,    85,    70,    86,    70,    87,
      71,    81,    70,    88,    71,    89,    70,    90,    71,    91,
      71,    92,    70,    93,    71,    94,    70,    95,    70,    96,
      30,    31,    97,    71,    98,    71,    99,    99,    44,    45,
     100,    71,   101,   102,    10,   104,    70,    77,    47,    48,
      49,    50,    51,   105,    70,   106,    70,   107,    52,    71,
     108,     6,    59,    61,    53,    66,    67,    69,    72,    12,
      46,    55,    72,    54,    67,    71,    75,    71,   103,    71,
      74,    10,    70,    76,    77,    71,    78,    76,    71,    79,
      71,    80
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 335 "rapic.y"
    {
  if (radar_verbose_flag) fprintf(stderr, "SUCCESSFUL parse\n");
  sprintf(radar->h.name, "%s", rh.namestr);
  sprintf(radar->h.radar_name, "%s", rh.namestr);

  radar = fill_header(radar);
  radar = RSL_prune_radar(radar);
  rapic_radar = radar;
  YYACCEPT;
}
    break;

  case 5:
#line 350 "rapic.y"
    {
  /* Attach the sweep to the volume. */
  if (radar_verbose_flag) fprintf(stderr, "Attach the sweep %d to the volume %d.\n",
		  isweep, ivolume);
  radar->v[ivolume]->sweep[isweep] = sweep;
  radar->v[ivolume]->h.f    = sweep->h.f;
  radar->v[ivolume]->h.invf = sweep->h.invf;
}
    break;

  case 6:
#line 360 "rapic.y"
    {
  /*  float c =  RSL_SPEED_OF_LIGHT; */
  if (rh.angle_resolution != 0) 
	sweep = RSL_new_sweep((int)(360.0/rh.angle_resolution+0.5));
  if (fabs(rh.elev - save_elev) > .5) { /* New sweep elevation. */
	isweep++;
	save_elev = rh.elev;
  }
  nray = 0;
  /* rapic_nyquist = c*((float)rh.prf/10.)/(4.*(float)rh.freq*100000.0); */
}
    break;

  case 8:
#line 375 "rapic.y"
    {
  if (radar_verbose_flag) fprintf(stderr, "sweepcount[0] = %d\n", sweepcount[0]);
  if (sweepcount[0] > 0) {
	radar->v[DZ_INDEX] = RSL_new_volume(sweepcount[0]);
	radar->v[DZ_INDEX]->h.type_str = strdup("Reflectivity");
  }
  if (radar_verbose_flag) fprintf(stderr, "sweepcount[1] = %d\n", sweepcount[1]);
  if (sweepcount[1] > 0) {
	volume = radar->v[VR_INDEX] = RSL_new_volume(sweepcount[1]);
	volume->h.type_str = strdup("Velocity");
	volume->h.calibr_const = 0.0;
  }
  if (radar_verbose_flag) fprintf(stderr, "sweepcount[2] = %d\n", sweepcount[2]);
  if (sweepcount[2] > 0) {
	radar->v[SW_INDEX] = RSL_new_volume(sweepcount[2]);
	volume->h.type_str = strdup("Spectral Width");
	volume->h.calibr_const = 0.0;
  }
  if (radar_verbose_flag) fprintf(stderr, "sweepcount[3] = %d\n", sweepcount[3]);
  if (sweepcount[3] > 0) {
	radar->v[ZD_INDEX] = RSL_new_volume(sweepcount[3]);
	volume->h.type_str = strdup("Reflectivity Depolarization Ratio");
	volume->h.calibr_const = 0.0;
  }
  if (radar_verbose_flag) fprintf(stderr, "sweepcount[4] = %d\n", sweepcount[4]);
  if (sweepcount[4] > 0) {
	radar->v[ZT_INDEX] = RSL_new_volume(sweepcount[4]);
	volume->h.type_str = strdup("Total Reflectivity");
	volume->h.calibr_const = 0.0;
  }
  isweep = -1; /* It keeps track of the sweep number across all field
                * types; volumes.  It is immediately bumped to 0 when
                * the sweepheader is parsed.
				*/
  save_elev = 99999;
}
    break;

  case 12:
#line 419 "rapic.y"
    {
  radar = RSL_new_radar(MAX_RADAR_VOLUMES);
  sweepcount[0] = 0;
  sweepcount[1] = 0;
  sweepcount[2] = 0;
  sweepcount[3] = 0;
  sweepcount[4] = 0;
  radar->h.number = atoi((yyvsp[(2) - (3)].token.s));
}
    break;

  case 14:
#line 431 "rapic.y"
    {
  if (atoi((yyvsp[(2) - (2)].token.s)) <= 0) {
	fprintf(stderr, "RAPIC: /IMAGESIZE == %d.  RAPIC ingest returning NULL.\n", atoi((yyvsp[(2) - (2)].token.s)));
	YYERROR;
  }
}
    break;

  case 16:
#line 441 "rapic.y"
    {
  ifield = atoi((yyvsp[(8) - (11)].token.s));
  sweepcount[ifield]++;
}
    break;

  case 20:
#line 456 "rapic.y"
    {

   /*   fprintf(stderr, "YACC len=%d text=<", yylval.token.len); */
   /*   binprint(yylval.token.s, yylval.token.len); */
   /*   fprintf(stderr, ">\n"); */

   /* Quiet the compilier, because I only use the rsl_f_list and rsl_invf_list. */
   RSL_ftype[0] = RSL_ftype[0];

   /* Use yylval.token.s and yylval.token.len */
   memset(outbuf, '\0', sizeof(outbuf));
   rapic_decode((unsigned char *)yylval.token.s, yylval.token.len, outbuf, &outbytes,
				&azim, &elev, &delta_time);
   /*   fprintf(stderr, "RAYDATA: ray %d, ivol %d, isweep %d, azim %f, elev %f, dtime %d, size=%d\n", nray, ivolume, isweep, azim, elev, delta_time, outbytes); */

   ray = RSL_new_ray(outbytes);
   rapic_load_ray_header(rh, nray, isweep, elev, azim, &ray->h); /* Mostly from the scanheader (rh). */
   ray->h.azimuth = azim;
   /*    if (39<azim && azim <40) { */
   ray->h.elev = elev;
   ray->h.sec += delta_time;
   ray->h.f    = RSL_f_list[ivolume]; /* Data conversion function. f(x). */
   ray->h.invf = RSL_invf_list[ivolume]; /* invf(x). */

   rapic_fix_time(ray);
   rapic_load_ray_data(outbuf, outbytes, ivolume, ray);
#define DODO
#undef DODO
#ifdef DODO
   if (ray->h.ray_num == 0 && ivolume == 1 && isweep == 0)
	 { int i;
   fprintf(stderr, "RAYDATA: ray %d, ivol %d, isweep %d, azim %f, elev %f, dtime %d, size=%d\n", nray, ivolume, isweep, azim, elev, delta_time, outbytes);
	 for (i=0; i<ray->h.nbins; i++) {
	   fprintf(stderr,"YACCray->range[%d] = %d  %f\n", i, (int)ray->range[i],
			   ray->h.f(ray->range[i]));
	 }
	 }
#endif
   /* Attach the ray to the sweep. */
   sweep->ray[nray]      = ray;
   sweep->h.beam_width   = ray->h.beam_width;
   sweep->h.vert_half_bw = sweep->h.beam_width / 2.0;
   sweep->h.horz_half_bw = sweep->h.beam_width / 2.0;
   sweep->h.sweep_num    = isweep;
   sweep->h.elev         = ray->h.elev;
   sweep->h.f            = ray->h.f;
   sweep->h.invf         = ray->h.invf;
   nray++;
   /*   } */
}
    break;

  case 24:
#line 515 "rapic.y"
    { memmove(rh.namestr,(yyvsp[(2) - (2)].token.s),(yyvsp[(2) - (2)].token.len)); }
    break;

  case 25:
#line 516 "rapic.y"
    { rh.country       = atoi((yyvsp[(2) - (2)].token.s)); }
    break;

  case 26:
#line 517 "rapic.y"
    { rh.station_id_no = atoi((yyvsp[(2) - (2)].token.s)); }
    break;

  case 27:
#line 518 "rapic.y"
    { rh.lat           = atof((yyvsp[(2) - (2)].token.s)); }
    break;

  case 28:
#line 519 "rapic.y"
    { rh.lon           = atof((yyvsp[(2) - (2)].token.s)); }
    break;

  case 29:
#line 520 "rapic.y"
    { rh.height        = atof((yyvsp[(2) - (2)].token.s)); }
    break;

  case 30:
#line 521 "rapic.y"
    { rh.datno         = atoi((yyvsp[(2) - (2)].token.s)); }
    break;

  case 31:
#line 522 "rapic.y"
    { rh.hhmm          = atof((yyvsp[(2) - (2)].token.s)); }
    break;

  case 32:
#line 523 "rapic.y"
    { memmove(rh.yyyymoddhhmmss,(yyvsp[(2) - (2)].token.s),(yyvsp[(2) - (2)].token.len)); }
    break;

  case 33:
#line 524 "rapic.y"
    { rh.versionNumber    = atof((yyvsp[(2) - (2)].token.s)); }
    break;

  case 34:
#line 525 "rapic.y"
    { rh.freq             = atoi((yyvsp[(2) - (2)].token.s)); }
    break;

  case 35:
#line 526 "rapic.y"
    { rh.prf              = atoi((yyvsp[(2) - (2)].token.s)); }
    break;

  case 36:
#line 527 "rapic.y"
    { rh.pulselen         = atof((yyvsp[(2) - (2)].token.s)); }
    break;

  case 37:
#line 528 "rapic.y"
    { rh.range_resolution = atoi((yyvsp[(2) - (2)].token.s)); }
    break;

  case 38:
#line 529 "rapic.y"
    { rh.anglerate        = atof((yyvsp[(2) - (2)].token.s)); }
    break;

  case 39:
#line 530 "rapic.y"
    { memmove(rh.clearair,(yyvsp[(2) - (2)].token.s),(yyvsp[(2) - (2)].token.len));}
    break;

  case 40:
#line 531 "rapic.y"
    { rh.angle_resolution = atof((yyvsp[(2) - (2)].token.s)); }
    break;

  case 41:
#line 532 "rapic.y"
    { rh.video_resolution = atoi((yyvsp[(2) - (2)].token.s)); }
    break;

  case 42:
#line 533 "rapic.y"
    { rh.start_range      = atoi((yyvsp[(2) - (2)].token.s)); }
    break;

  case 43:
#line 534 "rapic.y"
    { rh.end_range        = atoi((yyvsp[(2) - (2)].token.s)); }
    break;

  case 44:
#line 535 "rapic.y"
    { memmove(rh.product_type,(yyvsp[(2) - (3)].token.s),(yyvsp[(2) - (3)].token.len)); }
    break;

  case 47:
#line 538 "rapic.y"
    { rh.elev    = atof((yyvsp[(2) - (2)].token.s)); }
    break;

  case 48:
#line 539 "rapic.y"
    { rh.vellvl  = atof((yyvsp[(2) - (2)].token.s)); }
    break;

  case 49:
#line 541 "rapic.y"
    {
  rh.nyquist = atof((yyvsp[(2) - (2)].token.s));
  rapic_nyquist = rh.nyquist;
}
    break;

  case 50:
#line 545 "rapic.y"
    { memmove(rh.video,(yyvsp[(2) - (2)].token.s),(yyvsp[(2) - (2)].token.len)); }
    break;

  case 51:
#line 546 "rapic.y"
    { memmove(rh.imgfmt,(yyvsp[(2) - (2)].token.s),(yyvsp[(2) - (2)].token.len)); }
    break;

  case 89:
#line 592 "rapic.y"
    {rh.scannum = atoi((yyvsp[(1) - (1)].token.s));}
    break;

  case 90:
#line 593 "rapic.y"
    {rh.ofscans = atoi((yyvsp[(1) - (1)].token.s));}
    break;

  case 92:
#line 596 "rapic.y"
    {ivolume = DZ_INDEX; volume = radar->v[ivolume];}
    break;

  case 93:
#line 597 "rapic.y"
    {ivolume = VR_INDEX; volume = radar->v[ivolume];}
    break;

  case 94:
#line 598 "rapic.y"
    {ivolume = ZT_INDEX; volume = radar->v[ivolume];}
    break;

  case 95:
#line 599 "rapic.y"
    {ivolume = ZD_INDEX; volume = radar->v[ivolume];}
    break;

  case 96:
#line 600 "rapic.y"
    {ivolume = SW_INDEX; volume = radar->v[ivolume];}
    break;

  case 99:
#line 605 "rapic.y"
    {rh.ratio1 = 0; rh.ratio2 = 0;}
    break;

  case 100:
#line 606 "rapic.y"
    {rh.ratio1 = atoi((yyvsp[(1) - (3)].token.s)); rh.ratio2 = atoi((yyvsp[(3) - (3)].token.s));}
    break;


/* Line 1267 of yacc.c.  */
#line 1976 "rapic.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 610 "rapic.y"


int rapicerror(char *s)
{
  fprintf(stderr, "RAPIC ERROR: <%s> on token <", s);
  binprint(yylval.token.s, yylval.token.len);
  fprintf(stderr, ">\n");
  return 1;
}

int rapicwrap(char *s)
{
  yywrap(s);
  return 1;
}

