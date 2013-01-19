/*
    NASA/TRMM, Code 910.1.
    This is the TRMM Office Radar Software Library.
    Copyright (C) 1996, 1997, 1998
            John H. Merritt
            Space Applications Corporation
            Vienna, Virginia

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/
/*
 * 2/18/98 - John Merritt
 */
%{

#include <stdio.h>
#include "rapic_routines.h"
#include "rapic.h"
#include <string.h>

/*
     fprintf(stderr, "LEX len=%d text=<", yyleng);\
     binprint(yytext, yyleng);\
     fprintf(stderr, ">  token=[%d]\n", x);\
*/
#define strreturn(x) rapiclval.token.s = calloc(yyleng+1, sizeof(char));\
                     rapiclval.token.len=yyleng;\
                     memcpy(rapiclval.token.s, yytext, yyleng);\
                     return x;

%}


%x ATMODE

%%

\/IMAGE:          {strreturn(IMAGE);}
\/IMAGEEND:       {strreturn(IMAGEEND);}
\/IMAGESCANS:     {strreturn(IMAGESCANS);}
\/IMAGESIZE:      {strreturn(IMAGESIZE);}
\/SCAN            {strreturn(SCAN);}
"\/IMAGEHEADER END:"     {strreturn(IMAGEHEADEREND);}
COUNTRY:          {strreturn(COUNTRY);}
NAME:             {strreturn(NAME);}
STNID:            {strreturn(STNID);}
LATITUDE:         {strreturn(LATITUDE);}
LONGITUDE:        {strreturn(LONGITUDE);}
HEIGHT:           {strreturn(HEIGHT);}
DATE:             {strreturn(DATE);}
TIME:             {strreturn(TIME);}
TIMESTAMP:        {strreturn(TIMESTAMP);}
VERS:             {strreturn(VERS);}
FREQUENCY:        {strreturn(FREQUENCY);}
PRF:              {strreturn(PRF);}
PULSELENGTH:      {strreturn(PULSELENGTH);}
RNGRES:           {strreturn(RNGRES);}
ANGRES:           {strreturn(ANGRES);}
ANGLERATE:        {strreturn(ANGLERATE);}
CLEARAIR:         {strreturn(CLEARAIR);}
VIDRES:           {strreturn(VIDRES);}
STARTRNG:         {strreturn(STARTRNG);}
ENDRNG:           {strreturn(ENDRNG);}
PRODUCT:          {strreturn(PRODUCT);}
PASS:             {strreturn(PASS);}
IMGFMT:           {strreturn(IMGFMT);}
ELEV:             {strreturn(ELEV);}
VIDEO:            {strreturn(VIDEO);}
VELLVL:           {strreturn(VELLVL);}
NYQUIST:          {strreturn(NYQUIST);}
UNFOLDING:        {strreturn(UNFOLDING);}
VOLUMETRIC        {strreturn(VOLUMETRIC);}
NORMAL            {strreturn(NORMAL);}
[Nn][Oo][Nn][Ee]  {strreturn(NONE);}
[Oo][Ff]          {strreturn(OF);}
[Oo][Nn]          {strreturn(ON);}
[Oo][Ff][Ff]      {strreturn(OFF);}
[Rr][Ee][Ff][Ll]  {strreturn(REFL);}
[Vv][Ee][Ll]      {strreturn(VEL);}
[Uu][Nn][Cc][Oo][Rr][Rr][Ee][Ff][Ll]  {strreturn(UNCORREFL);}
[Zz][Dd][Rr]      {strreturn(ZDR);}
[Ww][Ii][Dd] |
[Ww][Ii][Dd][Tt][Hh]   {strreturn(WID);}

-?[[:digit:]]+       {strreturn(NUMBER);}
-?[[:digit:]]*[\.][[:digit:]]+ |
-?[[:digit:]]+[\.][[:digit:]]*  {strreturn(FLOATNUMBER);}
[[][[:digit:]]+[]] {strreturn(BRACKETNUM);}

[[:alpha:]]+       {strreturn(ALPHA);}

"END RADAR IMAGE"  {strreturn(ENDRADARIMAGE);}



"@" {BEGIN ATMODE; yymore();}
<ATMODE>. |
<ATMODE>\n      {yymore();}
<ATMODE>"\0\0@"  |
<ATMODE>"\0\0\x1a" {BEGIN INITIAL; yyless(yyleng-1); strreturn(RAYDATA);}

:  {return(yytext[0]);}

. ; /* Ignore. */
"\n" ; /* Ignore. */

%%
