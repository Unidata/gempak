/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

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




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 326 "rapic.y"
{
  Charlen token;
}
/* Line 1529 of yacc.c.  */
#line 161 "rapic.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE rapiclval;

