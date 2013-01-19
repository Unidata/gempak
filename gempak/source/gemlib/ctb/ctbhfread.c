#include "geminc.h"
#include "gemprm.h"

#define CTB_HERSHEY
#include "ctbcmn.h"

typedef struct {
    char	table_name[16];
    int		font_num;
} font_tbl_name_t;

void ctb_hfread ( int *iret )
/************************************************************************
 * ctb_pfread								*
 *									*
 * This function reads the info from the Hershey Fonts tables.		*
 * 									*
 * The Hershey Font coordinates are encoded using the ASCII characters.	*
 * Each character pair represents a coordinate in the drawing system	*
 * with 'R,R' as the origin. To get the coordinate values, subtract	*
 * 'R' from the given character. For example, the pair 'SB' corresponds	*
 * to (+1,-16).								*
 *									*
 * ctb_hfread ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *				  	  -1 = cannot open or read table*
 *				  	 -12 = cannot allocate memory	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	10/07	Created					*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    FILE	*fp;
    char	buffer[256], av[6];
    int		ii, nr, ier, ifont, ier2, jj, kk, nf, one = 1;

    font_tbl_name_t   tbl[] = {	{"hfont03.tbl",  3},
				{"hfont23.tbl", 23},
				{"hfont04.tbl",  4},
				{"hfont14.tbl", 14},
				{"hfont24.tbl", 24},
				{"hfont34.tbl", 34} };
/*---------------------------------------------------------------------*/

  *iret = G_NORMAL;

/*
 * Allocate space for all of the font structures.
 */
  nf = sizeof(tbl)/sizeof(tbl[0]);
  G_MALLOC ( _hfontTbl, HF_font_t, nf, "ctb_hfread - _hfontTbl");
  if ( _hfontTbl == NULL ) {
    *iret = -12;
    return;
  }

  _nhfont = 0;
  for ( ifont = 0; ifont < nf; ifont++ )  {
/*
 *  Read the font table and add entries into the structure. 
 */
    nr = 0;
    fp = cfl_tbop ( tbl[ifont].table_name, "hershey", &ier );
    if ( ier == 0 ) { 
	cfl_tbnr ( fp, &nr, &ier);
    }
	    
    if ( nr == 0 ) {
	cfl_clos ( fp, &ier );
        *iret = -1;
        return;
    }
    _hfontTbl[ifont].font_code = tbl[ifont].font_num;

/*
 *  Allocate space for the characters.
 */
    G_MALLOC ( _hfontTbl[ifont].character, HF_char_t, nr, "ctb_hfread - _hfontTbl[ifont].character");
    if ( _hfontTbl[ifont].character == NULL ) {
	cfl_clos ( fp, &ier );
	*iret = -12;
	return;
    }
  
/*
 * Read table entries into the structure.
 */
    ii = 0;

/* Read the next line from the file */
    cfl_trln ( fp, 256, buffer, &ier );

/*
 * Process the file while there is no error and
 * the end of the file has not been reached.
 */
    while ( ier >= 0 && ier != 4 ) {

        if ( ier == 0 ) {

	    G_MALLOC ( _hfontTbl[ifont].character[ii].point_code, char,  
		    strlen(buffer)+1, " ctb_hfread - _hfontTbl[ifont].character[ii].point_code");
	    if ( _hfontTbl[ifont].character[ii].point_code == NULL ) {
		cfl_clos ( fp, &ier );
		*iret = -12;
		return;
	    }
	    strcpy ( _hfontTbl[ifont].character[ii].point_code, buffer );

/* Get the character ASCII code */
	    cst_ncpy ( av, &(buffer[0]), 5, &ier2 );
	    cst_numb ( av, &(_hfontTbl[ifont].character[ii].ascii_val),
		       &ier2 );

/*
 * Get the number of points for the character
 * (The number in the file includes the X min and max,
 *  so subtract one)
 */
	    cst_ncpy ( av, &(buffer[5]), 3, &ier2 );
	    cst_numb ( av, &(_hfontTbl[ifont].character[ii].npts),
			&ier2 );
	    (_hfontTbl[ifont].character[ii].npts)--;

/* Get the X min and max */
	    _hfontTbl[ifont].character[ii].xmin = buffer[8] - 'R';
	    _hfontTbl[ifont].character[ii].xmax = buffer[9] - 'R';

/* Get the coordinates, if the number of points is > 0 */
	    if ( _hfontTbl[ifont].character[ii].npts > 0 ) {
		G_MALLOC ( _hfontTbl[ifont].character[ii].point, HF_point_t,
			_hfontTbl[ifont].character[ii].npts, 
			" ctb_hfread - _hfontTbl[ifont].character[ii].point");
                if ( _hfontTbl[ifont].character[ii].point == NULL ) {
		    cfl_clos ( fp, &ier );
		    *iret = -12;
		    return;
		}

/*
 * Check for " R", which denotes a "pen up".
 * If there is a pen up code, set the points to missing.
 * Otherwise, decode the coordinate values.
 */
		kk = 10;
		for ( jj = 0;
		      jj < _hfontTbl[ifont].character[ii].npts;
		      jj++ ) {
		    if ( strncmp ( &(buffer[kk]), " R", 2 ) == 0 ) {
			_hfontTbl[ifont].character[ii].point[jj].x = -99;
			_hfontTbl[ifont].character[ii].point[jj].y = -99;
			kk += 2;
		    }
		    else {
			_hfontTbl[ifont].character[ii].point[jj].x =
						buffer[kk] - 'R'; kk++;
			_hfontTbl[ifont].character[ii].point[jj].y =
						buffer[kk] - 'R'; kk++;
		    }
		}

	    }
/*
 * If there are no character codes,
 * set one point at the origin
 */
	    else {
		G_MALLOC ( _hfontTbl[ifont].character[ii].point, HF_point_t,
				one, " ctb_hfread - _hfontTbl[ifont].character[ii].point");
		if ( _hfontTbl[ifont].character[ii].point == NULL ) {
		    cfl_clos ( fp, &ier );
		    *iret = -12;
		    return;
		}
		_hfontTbl[ifont].character[ii].point[0].x = 0;
		_hfontTbl[ifont].character[ii].point[0].y = 0;
	    }
	    ii++;
        }

/* Read the next line from the file */
	cfl_trln ( fp, 256, buffer, &ier );

    }

/* Set the number of characters for the font */
    _hfontTbl[ifont].numchr = ii;

/* Close the font table */
    cfl_clos ( fp, &ier );

/* Increase the count for the number of fonts */
    _nhfont++;
  }  
}
