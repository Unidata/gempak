#include "geminc.h"
#include "gemprm.h"

void utf_plot ( char *filnam, int *zm, int *iret )
/************************************************************************
 * utf_plot								*
 *									*
 * This function plots the contents of a UTF file			*
 *									*
 * utf_plot ( filnam, zm, iret )					*
 *									*
 * Input parameters:							*
 *	*filnam		char		UTF filename			*
 *	*zm		int		Zoom threshold (from user)	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96	Copied and renamed from naf_plt		*
 * D. Keiser/GSC	 1/97	Fix error processing			*
 * D. Keiser/GSC	 6/97	Change zm to pointer, remove		*
 *				lenf from calling sequence		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 ***********************************************************************/
{
    int			add, offflg, ier, shift_x, shift_y, pi;
    int			gs, day, month, year;
    unsigned int	imax, jmax, imaxad, jmaxad;
    int			time, pdc, level, ier1;
    long		siz, size, bytesr;
    unsigned char	*pos, *endof, *file;
    char		grp[4], string[2];
    FILE		*fp;
/*---------------------------------------------------------------------*/
    *iret = 0;
    level = 0;
    strcpy(grp, "UTF");
    strcpy(string, " ");

/*
**  Determine the size of the UTF file.
*/
    utf_size( filnam, &size, &ier );
    file = (unsigned char *) malloc(size);

/*
**  Open the UTF file.
*/
    utf_open( filnam, (int *)&fp, &ier );
    if ( ier != 0 ) {
	er_lmsg ( &level, grp, &ier, filnam, &ier1, strlen(grp),
			strlen(filnam) );
    }
/*
**  Read the contents of the UTF file, then close the UTF file.
*/
    utf_read( fp, size, file, &bytesr, &ier );
    if ( ier != 0 ) {
	er_lmsg ( &level, grp, &ier, filnam, &ier1, strlen(grp),
			strlen(filnam) );
    }

    utf_clos( fp, &ier );

/*
**  Strip the UTF header and adjust the data for plotting purposes.
*/
    utf_strip( file, bytesr, &siz, &ier );
    if ( ier != 0 ) {
	er_lmsg ( &level, grp, &ier, filnam, &ier1, strlen(grp),
			strlen(filnam) );
    }

    endof = file + siz;
    pos = file;

/*
**  Plot the data contents of the UTF file.
*/
    while ( ( pos < endof ) && ( ier == 0 ) ) {

	offflg = G_FALSE;
	if ( *pos == 193 ) {
/*
**	    Graphic product definition (C1).
*/
	    utf_gphgd( pos, (int)siz, &shift_x, &shift_y, &pi, &gs, &imax,
		       &jmax, &imaxad, &jmaxad, &day, &month, &year,
		       &time, &pdc, &add, &ier );
	    if ( ier != 0 ) {
		er_lmsg ( &level, grp, &ier, string, &ier1, strlen(grp),
				strlen(string) );
	    }
	    pos += add;
	}

	else if ( *pos == 198 ) {
/*
**	    VEV vector (C6).
*/
	    utf_pvev( pos, &add, &ier );
	    if ( ier != 0 ) {
		er_lmsg ( &level, grp, &ier, string, &ier1, strlen(grp),
				strlen(string) );
	    }
	    pos += add;
	}

	else if ( *pos == 195 ) { 
/*
**	    Relative vector (C3).
*/
	    utf_pvctr( shift_x, shift_y, pos, &add, &ier );
	    if ( ier != 0 ) {
		er_lmsg ( &level, grp, &ier, string, &ier1, strlen(grp),
				strlen(string) );
	    }
	    pos += add;
	}

	else if ( *pos == 200 ) {
/*
**	    Offset text (C5).
*/
	    offflg = G_TRUE;
	    utf_ptext( offflg, shift_x, shift_y, endof, *zm, pos, &add,
		       &ier );
	    if ( ier != 0 ) {
		er_lmsg ( &level, grp, &ier, string, &ier1, strlen(grp),
				strlen(string) );
	    }
	    pos += add;
	}

	else if ( *pos == 197 ) {
/*
**	    Regular text (C8).
*/
	    utf_ptext( offflg, shift_x, shift_y, endof, *zm, pos, &add,
		       &ier );
	    if ( ier != 0 ) {
		er_lmsg ( &level, grp, &ier, string, &ier1, strlen(grp),
				strlen(string) );
	    }
	    pos += add;
	} 

	else
	    pos += 1;

    }
    free(file);

}
