#include "geminc.h"
#include "gemprm.h"

#define GSKY	1
#define GPWX	2
#define GPTN	3
#define GWTH	4
#define GCTP	5
#define GSPC	6
#define GTRB	7
#define GICE	8


void utf_ptext ( int offflg, int shift_x, int shift_y,
		 unsigned char *endof, int zm, unsigned char *ptr,
		 int *add, int *iret )
/************************************************************************
 * utf_ptext								*
 *									*
 * This function plots both regular and offset text records C5 and C8	*
 * from a UTF file.							*
 *									*
 * utf_ptext ( offflg, shift_x, shift_y, endof, zm, ptr, add, iret )	*
 *									*
 * Input parameters:							*
 *	offflg		int		Offset vs regular text flag	*
 *	shift_x		int		X shift factor			*
 *	shift_y		int		Y shift factor			*
 *	*endof		unsigned char	End of buffer			*
 *	zm		int		Zoom threshold (from user)	*
 *	*ptr		unsigned char	Position in buffer		*
 *									*
 * Output parameters:							*
 *	*add		int		Total number of bytes in record	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96	Copied from utf_text			*
 * D. Keiser/GSC	 4/97	Corrected turbulence plot		*
 * S. Jacobs/NCEP	 4/97	Removed value from turb sym		*
 * D. Keiser/GSC	 6/97	Initialized txtwid			*
 * S. Jacobs/NCEP	 7/97	Changed font size calculation		*
 * D. Keiser/GSC	 7/97	Increased string sizes			*
 * S. Jacobs/NCEP	 8/97	Corrected PTND symbol numbers		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * S. Jacobs/NCEP	10/97	Changed the calling seq for gstext	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 7/98	Changed the font type and size		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 ***********************************************************************/
{
    int 		len, offset, sign, i, j, delx, dely, ixoff, g, b;
    int			iyoff, spcflg, np, ier, rb, ipnt, pltfil, bytadd;
    int			jpnt, zt, zf, chrsiz, txtwid, fntnum, txtflg, nc;
    int 		ipwwid, isktyp, iskwid, iptwid, iwtwid, ictwid;
    int			ispwid, icewid, nchr, textfg, ituwid, numpnt;
    int			idum;
    float 		sf, txtsiz, temp, rotat, rx, ry, szsky, szpwth;
    float		rcode, xl, yb, xr, yt, xl2, yb2, xr2;
    float		yt2, xrat, yrat, rdelx, rdely, szptnd, szwthr;
    float		szctyp, szspcl, szicng, szturb, xin[2], yin[2];
    float 		xout[2], yout[2];
    unsigned char 	string[200], *tmpbuf=NULL, outstr[200];
    char 		newstr[200];

    int gfunc[] = { 0000, GSKY, GSKY, GSKY, GSKY, GSKY, GSPC, GSPC,
   		    0000, 0000, 0000, 0000, 0000, 0000, GSKY, GSKY,
		    GSKY, 0000, 0000, GSKY, GSKY, GSKY, GPTN, GPTN, 
		    GPTN, GPTN, GPTN, GPTN, GWTH, GWTH, GWTH, GWTH,
		    GWTH, GWTH, GWTH, GWTH, GWTH, GWTH, GWTH, GWTH,
		    GWTH, GPWX, GWTH, GWTH, GWTH, GWTH, GWTH, GWTH,
		    GSPC, GSPC, GSPC, GSPC, GSPC, GWTH, GPWX, GWTH,
		    GSPC, GSPC, GWTH, GWTH, GWTH, GWTH, GWTH, GWTH,
		    GWTH, GWTH, GWTH, GWTH, GWTH, GWTH, GWTH, GWTH,
		    GWTH, GWTH, GWTH, GWTH, GWTH, GWTH, GWTH, GWTH,
		    GSPC, GSPC, GSPC, GWTH, GSPC, GSPC, GSPC, GSPC,
		    GWTH, GWTH, GSPC, GSPC, GSPC, GCTP, GCTP, GCTP,
		    GCTP, GCTP, GCTP, GCTP, GCTP, GCTP, GCTP, GCTP, 
		    GCTP, GCTP, GCTP, GCTP, GCTP, GCTP, GCTP, GCTP, 
		    GCTP, GCTP, GCTP, GCTP, GCTP, GCTP, GCTP, GCTP,
		    GSPC, GSPC, GTRB, GTRB, GTRB, GICE, GICE, GICE };

    int gsym[] = { 0, 6, 7, 8, 9, 10, 0, 8,
    		   0, 0, 0, 0, 0, 0, 0, 1,
		   2, 0, 0, 3, 4, 5, 999, 1999,
		   5999, 6999, 3999, 8999, 4, 5, 6, 7,
		   8, 9, 10, 11, 12, 13, 14, 15,
		   16, 9, 18, 19, 50, 60, 70, 68,
		   30, 14, 16, 15, 4, 45, 8, 31,
		   18, 17, 34, 36, 37, 38, 39, 41,
		   44, 48, 49, 52, 56, 57, 66, 67,
		   47, 58, 76, 77, 78, 79, 81, 62,
		   4, 19, 20, 43, 21, 23, 22, 24,
		   97, 72, 12, 13, 29, 11, 12, 13, 
		   14, 15, 16, 17, 18, 19, 1, 2,
		   3, 4, 5, 6, 7, 8, 9, 21,
		   22, 23, 24, 25, 26, 27, 28, 29,
		   25, 26, 2, 4, 6, 3, 5, 8 };

/*---------------------------------------------------------------------*/
    *iret = 0;
    sf = 0.25F;
    temp = 0.8F;
    fntnum = 21;
    txtflg = 2;
    ixoff = 0;
    iyoff = 0;
    rotat = 0.0F;
    delx = 0;
    dely = 0;
    txtwid = 1;

/*
**  Decode the header of the C5 or C8 record.
*/
    utf_dtext( offflg, shift_x, shift_y, zm, endof, ptr, &g, &b, &rb,
	       &zt, &pltfil, &zf, &chrsiz, &ipnt, &jpnt, &len, &bytadd,
	       &ier );
    *add = bytadd;
    ptr += 6;

/*
**  If offset text record C8, decode delta X and delta Y.
*/
    if ( offflg ) {
     	for ( i = 0; i < 2; i++, ptr++ ) {
    	    sign = *ptr & 0x80;
      	    offset = *ptr & 0x7f;
      	    if ( sign != 0 )
            	offset -= 0x80;
      	    if ( i == 0 )
	    	delx = offset << shift_x;
      	    else
	     	dely = offset << shift_y;
    	}
    }

/*
**  Query MAP and DEVICE bounds and compute X and Y ratios that
**  will adjust delta X and delta Y for zooming. Add delta X and 
**  delta Y to I, J.
*/
    gqbnd( sys_M, &xl, &yb, &xr, &yt, &ier, strlen(sys_M) );
    xin[0] = xl;
    xin[1] = xr;
    yin[0] = yb;
    yin[1] = yt;
    numpnt = 2;
    gtrans( sys_M, sys_G, &numpnt, xin, yin, xout, yout, &ier, 
					strlen(sys_M),	strlen(sys_G) );
    gqbnd( sys_D, &xl2, &yb2, &xr2, &yt2, &ier, strlen(sys_D) );
    xrat = (xout[1] - xout[0]) / (xr2 - xl2) / 2.0F;
    yrat = (yout[1] - yout[0]) / (yb2 - yt2) / 2.0F;
    rx = (float) ipnt;
    ry = (float) jpnt; 
    rdelx = (float) delx;
    rdely = (float) dely;
    rx = rx + (rdelx + 3.0F) * xrat;
    ry = ry + (rdely + 15.0F) * yrat;

    spcflg = G_FALSE;
    textfg = G_FALSE;
    nchr = 0;

/*
**  Traverse string of characters, plotting only if plot filter is true.
*/
    while ( (*ptr < 128) && (ptr < endof) ) {
	if ( pltfil ) {

/*
**	    If a cursor moving character is encountered and textfg is
**	    TRUE, build string from buffer and pass into GTEXT to be
**	    plotted.
*/
	    if ( ( *ptr == 18 ) || (( *ptr >= 8 ) && ( *ptr <= 13 ) ) ) {
		if ( textfg ) {
		    j = 0;
		    for ( i = 0; i < nchr; i++, tmpbuf++) {
		    	if ( ( *tmpbuf >= 32 ) && ( *tmpbuf < 127 ) ) {
			    string[j] = *tmpbuf;
			    j++;
			}
		    }
		    string[j] = CHNULL;
		    if  ( chrsiz < 3 )  {
			txtsiz = 1.000F;
		    }
		    else  {
			txtsiz = 1.714F;
		    }
		    idum = 0;
		    gstext( &fntnum, &txtflg, &txtsiz, &txtwid,
			    &idum, &idum, &idum, &ier );
		    cst_unpr( (char *)string, (char *)outstr, &ier );
		    strcpy(newstr, (char *) outstr);
		    cst_lstr( newstr, &nc, &ier );
		    if ( nc > 0 ) {
			gtext( sys_G, &rx, &ry, newstr, &rotat, &ixoff,
			       &iyoff, &ier, strlen(sys_G), 
			       strlen(newstr) );
			ixoff = nc * 3;
		    }
		    textfg = G_FALSE;
		    nchr = 0;
		}

/*
**		Check for special characters and set appropriate GEMPAK
**		offsets.
*/
		switch(*ptr) {

/*
**		    Back space.
*/
		    case 8:
			ixoff -= 2;
			break;

/*
**		    Forward space. 
*/
		    case 9:
			ixoff += 2;
			break;

/*
**		    Down space. 
*/
		    case 10:
			iyoff -= 2;
			break;

/*
**		    Up space. 
*/
		    case 11:
			iyoff += 2; 
			break;

/*
**		    New Line (1.5). 
*/
		    case 12:
			ixoff = 0;
			iyoff -= 3;
			break;

/*
**		    New Line (1.0). 
*/
		    case 13:
			ixoff = 0;
			iyoff -= 2;
			break;

/*
**		    Set special character flag. 
*/
		    case 18:
			spcflg = G_TRUE;	
			break;
		}
	    }

/*
**	    If a reset to regular character mode character is
**	    encountered, set the special character flag back to FALSE.
*/
	    else if ( *ptr == 17 ) {
		spcflg = G_FALSE;
	    }

/*
**	    If a special character is encountered, check it against the
**	    symbol array, and plot it according to which symbol code it
** 	    represents.
*/
	    else {
		if ( spcflg ) {
		    np = 1;
		    switch (gfunc[*ptr]) {
/*
**			Sky cover. 
*/
			case GSKY:
			    isktyp = 0;
			    iskwid = 0;
			    szsky = temp + ((float) chrsiz*sf) - 0.05F;
			    rcode = (float) gsym[*ptr];
			    gssky( &szsky, &isktyp, &iskwid, &ier );
			    gsky( sys_G, &np, &rcode, &rx, &ry, &ixoff,
				  &iyoff, &ier, strlen(sys_G) );
			    break;

/*
**			Past weather. 
*/
			case GPWX:
			    ipwwid = 0;
			    szpwth = temp + ((float) chrsiz*sf);
		    	    rcode = (float) gsym[*ptr];
		    	    gspwth( &szpwth, &ipwwid, &ier );
		    	    gpwth( sys_G, &np, &rcode, &rx, &ry, &ixoff,
				   &iyoff, &ier, strlen(sys_G) );
		    	    break;

/*
**			Pressure tendency. 
*/
			case GPTN:
			    iptwid = 0;
			    szptnd = temp + ((float) chrsiz*sf);
		    	    rcode = (float) gsym[*ptr];
			    gsptnd( &szptnd, &iptwid, &ier );
			    gptnd( sys_G, &np, &rcode, &rx, &ry, &ixoff,
				   &iyoff, &ier, strlen(sys_G) );
			    break;

/*
**			WMO weather symbols. 
*/
			case GWTH:
			    iwtwid = 0;
			    if ( gsym[*ptr] == 70 || gsym[*ptr] == 72 ) 
				szwthr = temp + ((float) chrsiz*sf) +
								0.5F;
			    else
				szwthr = temp + ((float) chrsiz*sf);
		    	    rcode = (float) gsym[*ptr];
			    gswthr( &szwthr, &iwtwid, &ier );
			    gwthr( sys_G, &np, &rcode, &rx, &ry, &ixoff,
				   &iyoff, &ier, strlen(sys_G) );
			    break;

/*
**			Cloud type. 
*/
			case GCTP:
			    ictwid = 0;
			    szctyp = temp + ((float) chrsiz*sf);
		    	    rcode = (float) gsym[*ptr];
			    gsctyp( &szctyp, &ictwid, &ier );
			    gctyp( sys_G, &np, &rcode, &rx, &ry, &ixoff,
				   &iyoff, &ier, strlen(sys_G) );
			    break;

/*
**			Special symbols. 
*/
			case GSPC:
			    ispwid = 0;
			    if ( gsym[*ptr] == 12 || gsym[*ptr] == 13 )
				szspcl = temp + ((float) chrsiz*sf) -
								0.3F;
			    else if ( gsym[*ptr] == 0 ) 
				szspcl = temp + ((float) chrsiz*sf) -
								0.4F;
			    else
				szspcl = temp + ((float) chrsiz*sf);
			    rcode = (float) gsym[*ptr];
			    gsspcl( &szspcl, &ispwid, &ier );
			    gspcl( sys_G, &np, &rcode, &rx, &ry, &ixoff,
				   &iyoff, &ier, strlen(sys_G) );
			    break;

/*
**			Turbulence. 
*/
			case GTRB:
			    ituwid = 0;
			    szturb = temp + ((float) chrsiz*sf) - 0.5F;
		    	    rcode = (float) gsym[*ptr];
			    gsturb( &szturb, &ituwid, &ier );
			    gturb( sys_G, &np, &rcode, &rx, &ry, &ixoff,
				   &iyoff, &ier, strlen(sys_G) );
			    break;

/*
**			Icing. 
*/
			case GICE:
			    icewid = 0;
			    szicng = temp + ((float) chrsiz*sf) - 0.5F;
		    	    rcode = (float) gsym[*ptr];
			    gsicng( &szicng, &icewid, &ier );
			    gicng( sys_G, &np, &rcode, &rx, &ry, &ixoff,
				   &iyoff, &ier, strlen(sys_G) );
			    break;

		    }
		    ixoff += 2;
		}

/*
**		If textfg is FALSE, place character in buffer to be used
**		in later building the string to plot.
*/
		else {
		    if ( !textfg ) {
			tmpbuf = ptr;
			textfg = G_TRUE;
		    }
		    nchr++;
		}
	    }
	}
    ptr++;
    }

/*
**  Build string from buffer and pass into GTEXT to be plotted.
*/
    if ( textfg ) {
	if ( pltfil ) {
	j = 0;
	for ( i = 0; i < nchr; i++, tmpbuf++) {
	    if ( ( *tmpbuf >= 32 ) && ( *tmpbuf < 127 ) ) {
		string[j] = *tmpbuf;
		j++;
	    }
	}
	string[j] = CHNULL;
	if  ( chrsiz < 3 )  {
	    txtsiz = 1.000F;
	}
	else  {
	    txtsiz = 1.714F;
	}
	idum = 0;
       	gstext( &fntnum, &txtflg, &txtsiz, &txtwid, 
		&idum, &idum, &idum, &ier );
	    cst_unpr( (char *)string, (char *)outstr, &ier );
	    strcpy(newstr, (char *) outstr);
	    cst_lstr( newstr, &nc, &ier );
	    if  ( nc > 0 ) {
		gtext( sys_G, &rx, &ry, newstr, &rotat, &ixoff,
		       &iyoff, &ier, strlen(sys_G), strlen(newstr) );
		ixoff = nc * 3;
	    }
	    textfg = G_FALSE;
	    nchr = 0;
	}
    }
}
