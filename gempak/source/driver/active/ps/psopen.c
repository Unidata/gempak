#include "pscmn.h"

void psopen ( int *iret )
/************************************************************************
 * psopen								*
 *									*
 * This subroutine opens a PostScript file.				*
 *									*
 * psopen ( iret )			               			*
 *                                                                      *
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Chang/EAI		 2/94  						*
 * E. Wehner/EAI	 2/96	Add logic for filename, paper size	*
 * S. Jacobs/NCEP	 4/96	Changed to use calls to CFL functions	*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 * S. Jacobs/NCEP	11/97	Added LT, RT and CT commands		*
 * S. Jacobs/NCEP	 3/98	Added fill pattern definitions		*
 * S. Jacobs/NCEP	 8/98	Changed plot dim to leave 1/4 in margin	*
 * S. Jacobs/NCEP	 9/98	Added large paper 24x36 inches		*
 * A. Hardy/GSC		11/98	Added A command                         *
 * S. Jacobs/NCEP	 8/99	Changed page size dictionary to PS lev 3*
 * m.gamazaychikov/SAIC 01/03	added new fill patterns 4 through 7   	*
 ***********************************************************************/
{

	char	prolog[10000], buff[80];
	int	ierr, lenp;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

 /*     
  *	If no file name is specified, return with an error.
  */
	if ( strlen ( filnam ) == (size_t)0 )
	{
	    *iret = G_NOPSFL;
	    return;
	}

/*
 *	If the open fails, return immediately.
 */
	flun = cfl_wopn ( filnam, &ierr );
	if  ( ierr != 0 )
	{
	    *iret  = G_NOPSFL;
	    opnfil = G_FALSE;
	    return;
	}

/*
 *	Mark file as opened.
 */
	opnfil = G_TRUE;
	psplot = G_FALSE;

/*
 *	Construct the file prolog.
 */
	strcpy ( prolog, "%!PS-Adobe-3.0\n"); 
	strcat ( prolog, "%%Title:\n"); 
	strcat ( prolog, "%%Creator: GEMPAK\n"); 
	strcat ( prolog, "%%EndComments\n"); 
	strcat ( prolog, "%%DocumentsFont:(atend) \n"); 
	strcat ( prolog, "%%Pages:(atend)\n"); 

	strcat ( prolog, " /M {moveto} def\n"); 
	strcat ( prolog, " /L {lineto} def\n"); 
	strcat ( prolog, " /N {newpath} def\n"); 
	strcat ( prolog, " /LW {setlinewidth} def\n"); 
	strcat ( prolog, " /A {arc} def\n"); 
	strcat ( prolog, " /AF {arc fill} def\n"); 

	strcat ( prolog, " /MS {makefont setfont} def\n"); 
	strcat ( prolog, " /FF {findfont} def\n"); 
	strcat ( prolog, " /CF {currentfont} def\n"); 
	strcat ( prolog, " /FL {closepath fill} def\n"); 
	strcat ( prolog, " /RGB {setrgbcolor} def\n"); 
	strcat ( prolog, " /LT { 3 -1 roll 3 -1 roll M show } def\n");
	strcat ( prolog, " /CT { dup stringwidth pop 2 div 4 -1 roll sub neg 3 -1 roll M show } def\n");
	strcat ( prolog, " /RT { dup stringwidth pop 4 -1 roll sub neg 3 -1 roll M show } def\n");
	
	strcat ( prolog, " /inch {72 mul} def\n");

	strcat ( prolog, " /papersizedict 10 dict def\n" );
	strcat ( prolog, " /papersize {\n" );
	strcat ( prolog, "    papersizedict begin\n" );
	strcat ( prolog, "        /Letter { << /PageSize [612 792] >> setpagedevice } def\n" );
	strcat ( prolog, "        /Tabloid { << /PageSize [792 1224] >> setpagedevice } def\n" );
	strcat ( prolog, "        /Roll1 {<< /PageSize [1584 2448] >> setpagedevice } def\n" );
	strcat ( prolog, "        /Roll2 {<< /PageSize [2448 3168] >> setpagedevice } def\n" );
	strcat ( prolog, "        /unknown {unknown} def\n" );
	strcat ( prolog, "    papersizedict dup papername get\n" );
	strcat ( prolog, "    end\n" );
	strcat ( prolog, "    /pagedicttop countdictstack 1 add def\n" );
	strcat ( prolog, "    statusdict begin stopped end\n" );
	strcat ( prolog, "    countdictstack -1 pagedicttop {pop end} for\n" );
	strcat ( prolog, "    } def\n" );

	sprintf ( buff, " /papername {%s} def\n", pprnam );
	strcat ( prolog, buff );

/*
 *	Setup for fill patterns.
 *	Fill pattern 1 is reserved for solid fill.
 */

/*
 *	Fill pattern 2 - dashed line
 */
	strcat ( prolog, " /pat2dict 10 dict def\n" );
	strcat ( prolog, " /pat2 {\n" );
	strcat ( prolog, "     pat2dict begin\n" );
	strcat ( prolog, "     /PaintType 2 def\n" );
	strcat ( prolog, "     /PatternType 1 def\n" );
	strcat ( prolog, "     /TilingType 1 def\n" );
	strcat ( prolog, "     /BBox [ -8 -8 8 8 ] def\n" );
	strcat ( prolog, "     /XStep 15 def\n" );
	strcat ( prolog, "     /YStep 15 def\n" );
	strcat ( prolog, "     /PaintProc {\n" );
	strcat ( prolog, "         pop\n" );
	if  ( landscape )  {
	    strcat ( prolog, "         90 rotate\n" );
	}
	strcat ( prolog, "         -4  4 M -4  5 L -1 8 L 0 8 L FL\n" );
	strcat ( prolog, "          4  4 M  4  5 L  7 8 L 8 8 L FL\n" );
	strcat ( prolog, "         -4 -4 M -4 -3 L -1 0 L 0 0 L FL\n" );
	strcat ( prolog, "          4 -4 M  4 -3 L  7 0 L 8 0 L FL\n" );
	strcat ( prolog, "         } def\n" );
	strcat ( prolog, "     pat2dict end\n" );
	strcat ( prolog, "     } def\n" );
	strcat ( prolog, " pat2 matrix makepattern /pattern2 exch def\n" );
/*
 *	Fill pattern 3 - coarse-spacing straight line
 */
	strcat ( prolog, " /pat3dict 10 dict def\n" );
	strcat ( prolog, " /pat3 {\n" );
	strcat ( prolog, "     pat3dict begin\n" );
	strcat ( prolog, "     /PaintType 2 def\n" );
	strcat ( prolog, "     /PatternType 1 def\n" );
	strcat ( prolog, "     /TilingType 1 def\n" );
	strcat ( prolog, "     /BBox [ -8 -8 8 8 ] def\n" );
	strcat ( prolog, "     /XStep 15 def\n" );
	strcat ( prolog, "     /YStep 15 def\n" );
	strcat ( prolog, "     /PaintProc {\n" );
	strcat ( prolog, "         pop\n" );
	if  ( landscape )  {
	    strcat ( prolog, "         90 rotate\n" );
	}
	strcat ( prolog, "         -8 -8 M -8 -7 L 7 8 L 8 8 L FL\n" );
	strcat ( prolog, "         } def\n" );
	strcat ( prolog, "     pat3dict end\n" );
	strcat ( prolog, "     } def\n" );
	strcat ( prolog, " pat3 matrix makepattern /pattern3 exch def\n" );
/*
 *      Fill pattern 4 - medium-spacing straight line
 */
        strcat ( prolog, " /pat4dict 10 dict def\n" );
        strcat ( prolog, " /pat4 {\n" );
        strcat ( prolog, "     pat4dict begin\n" );
        strcat ( prolog, "     /PaintType 2 def\n" );
        strcat ( prolog, "     /PatternType 1 def\n" );
        strcat ( prolog, "     /TilingType 1 def\n" );
        strcat ( prolog, "     /BBox [ -8 -8 8 8 ] def\n" );
        strcat ( prolog, "     /XStep 15 def\n" );
        strcat ( prolog, "     /YStep 15 def\n" );
        strcat ( prolog, "     /PaintProc {\n" );
        strcat ( prolog, "         pop\n" );
        if  ( landscape )  {
            strcat ( prolog, "         90 rotate\n" );
        }
        strcat ( prolog, "         -8  0 M -8  1 L -1 8 L 0 8 L FL\n" );
        strcat ( prolog, "         -8 -8 M -8 -7 L  7 8 L 8 8 L FL\n" );
        strcat ( prolog, "         -1 -8 M -1 -7 L  7 1 L 8 1 L FL\n" );
        strcat ( prolog, "         } def\n" );
        strcat ( prolog, "     pat4dict end\n" );
        strcat ( prolog, "     } def\n" );
        strcat ( prolog, " pat4 matrix makepattern /pattern4 exch def\n" );
/*
 *      Fill pattern 5 - zigzag 
 */
        strcat ( prolog, " /pat5dict 10 dict def\n" );
        strcat ( prolog, " /pat5 {\n" );
        strcat ( prolog, "     pat5dict begin\n" );
        strcat ( prolog, "     /PaintType 2 def\n" );
        strcat ( prolog, "     /PatternType 1 def\n" );
        strcat ( prolog, "     /TilingType 1 def\n" );
        strcat ( prolog, "     /BBox [ -8 -8 8 8 ] def\n" );
        strcat ( prolog, "     /XStep 15 def\n" );
        strcat ( prolog, "     /YStep 15 def\n" );
        strcat ( prolog, "     /PaintProc {\n" );
        strcat ( prolog, "         pop\n" );
        if  ( landscape )  {
            strcat ( prolog, "         90 rotate\n" );
        }
/*
 *       strcat ( prolog, "         -8  0 M -8  1 L -5  1 L -5  5 L -1  5 L -1 8 L 0 8 L FL\n" );
 *       strcat ( prolog, "         -8 -8 M -5 -7 L -5 -3 L -1 -3 L -1  1 L 3 1 L 3 5 L 7 5 L  7 8 L 8 8 L FL\n" );
 *       strcat ( prolog, "         -1 -8 M -1 -7 L  2 -7 L  2 -3 L  6 -3 L 6 0 L 8 0 L FL\n" );
 */
        strcat ( prolog, "         -7 -8 M -8 -8 L -8 -4 L -7 -4 L -7 -8 L FL\n" );
        strcat ( prolog, "          1 -5 M -8 -5 L -8 -4 L  1 -4 L  1 -5 L FL\n" );
        strcat ( prolog, "          1 -5 M  0 -5 L  0  4 L  1  4 L  1 -5 L FL\n" );
        strcat ( prolog, "          8  3 M  0  3 L  0  4 L  8  4 L  8  3 L FL\n" );
        strcat ( prolog, "          8  3 M  7  3 L  7  8 L  8  8 L  8  3 L FL\n" );
        strcat ( prolog, "         } def\n" );
        strcat ( prolog, "     pat5dict end\n" );
        strcat ( prolog, "     } def\n" );
        strcat ( prolog, " pat5 matrix makepattern /pattern5 exch def\n" );
/*
 *      Fill pattern 6 - dotted pattern
 */
        strcat ( prolog, " /pat6dict 10 dict def\n" );
        strcat ( prolog, " /pat6 {\n" );
        strcat ( prolog, "     pat6dict begin\n" );
        strcat ( prolog, "     /PaintType 2 def\n" );
        strcat ( prolog, "     /PatternType 1 def\n" );
        strcat ( prolog, "     /TilingType 1 def\n" );
        strcat ( prolog, "     /BBox [ -8 -8 8 8 ] def\n" );
        strcat ( prolog, "     /XStep 15 def\n" );
        strcat ( prolog, "     /YStep 15 def\n" );
        strcat ( prolog, "     /PaintProc {\n" );
        strcat ( prolog, "         pop\n" );
        if  ( landscape )  {
            strcat ( prolog, "         90 rotate\n" );
        }
        strcat ( prolog, "         N -6  2 1 0 360 AF\n" );
        strcat ( prolog, "         N -2  6 1 0 360 AF\n" );
        strcat ( prolog, "         N -6 -6 1 0 360 AF\n" );
        strcat ( prolog, "         N -2 -2 1 0 360 AF\n" );
        strcat ( prolog, "         N  2  2 1 0 360 AF\n" );
        strcat ( prolog, "         N  6  6 1 0 360 AF\n" );
        strcat ( prolog, "         N  2 -6 1 0 360 AF\n" );
        strcat ( prolog, "         N  6 -2 1 0 360 AF\n" );
        strcat ( prolog, "         } def\n" );
        strcat ( prolog, "     pat6dict end\n" );
        strcat ( prolog, "     } def\n" );
        strcat ( prolog, " pat6 matrix makepattern /pattern6 exch def\n" );
/*
 *      Fill pattern 7 - fine-spacing straight line
 */
        strcat ( prolog, " /pat7dict 10 dict def\n" );
        strcat ( prolog, " /pat7 {\n" );
        strcat ( prolog, "     pat7dict begin\n" );
        strcat ( prolog, "     /PaintType 2 def\n" );
        strcat ( prolog, "     /PatternType 1 def\n" );
        strcat ( prolog, "     /TilingType 1 def\n" );
        strcat ( prolog, "     /BBox [ -8 -8 8 8 ] def\n" );
        strcat ( prolog, "     /XStep 15 def\n" );
        strcat ( prolog, "     /YStep 15 def\n" );
        strcat ( prolog, "     /PaintProc {\n" );
        strcat ( prolog, "         pop\n" );
        if  ( landscape )  {
            strcat ( prolog, "         90 rotate\n" );
        }
        strcat ( prolog, "         -8  4 M -8  5 L -5  8 L -4  8 L FL\n" );
        strcat ( prolog, "         -8  0 M -8  1 L -1  8 L  0  8 L FL\n" );
        strcat ( prolog, "         -8 -4 M -8 -3 L  3  8 L  4  8 L FL\n" );
        strcat ( prolog, "         -8 -8 M -8 -7 L  7  8 L  8  8 L FL\n" );
        strcat ( prolog, "         -5 -8 M -5 -7 L  7  5 L  8  5 L FL\n" );
        strcat ( prolog, "         -1 -8 M -1 -7 L  7  1 L  8  1 L FL\n" );
        strcat ( prolog, "          3 -8 M  3 -7 L  7 -3 L  8 -3 L FL\n" );
        strcat ( prolog, "         } def\n" );
        strcat ( prolog, "     pat7dict end\n" );
        strcat ( prolog, "     } def\n" );
        strcat ( prolog, " pat7 matrix makepattern /pattern7 exch def\n" );

	strcat ( prolog, "%%EndProlog\n" );
	strcat ( prolog, "%%BeginSetup\n" );
	strcat ( prolog, " papersize\n" );
	strcat ( prolog, "%%EndSetup\n" );

	strcat ( prolog, " save\n");

/*
 *	Put transformation info in the PS header as well.
 */
	if  ( strncmp ( pprnam, "/Roll", 5 ) == 0 )  {
	    if  ( landscape )
	    {
		sprintf ( buff,
		    " %5.2f inch .67 inch translate 90 rotate 0.03125 0.03125 scale\n", ysize+0.60 );
	    }
	    else
	    {
		sprintf ( buff,
		    " .60 inch .67 inch translate 0.03125 0.03125 scale\n" );
	    }
	}
	else {
	    if  ( landscape )
	    {
		sprintf ( buff,
		    " %5.2f inch .25 inch translate 90 rotate 0.03125 0.03125 scale\n", ysize-0.25 );
	    }
	    else
	    {
		sprintf ( buff,
		    " .25 inch .25 inch translate 0.03125 0.03125 scale\n" );
	    }
	}
 
	strcat ( prolog, buff );
	strcat ( prolog, " 1 setlinecap 1 setlinejoin newpath\n"); 
	strcat ( prolog, "%%Page:   1  ?\n\0");

/*
 *	Write prolog commands to file.	    
 */
	lenp = strlen ( prolog );
	cfl_writ ( flun, lenp, (unsigned char *)prolog, &ierr );

	nnpage = 1;
	iawdth = 0;

/*
 *	Set the color components in the next file.
 */
	resetc = G_TRUE;

}
