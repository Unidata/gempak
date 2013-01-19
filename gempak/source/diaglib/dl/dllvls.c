#include "lyrdiag.h"

#define	NMAN  11

void dl_lvls ( char **uargs, int nargs, char *glvl, int *nlev, int *iret )
/************************************************************************
 * dl_lvls								*
 *									*
 * This subroutine scans LYR_ function arguments and GLEVEL input, and 	*
 * set the layer common block.						*
 *									*
 * dl_lvls  ( uargs, nargs, glvl, nlev, iret )				*
 *									*
 * Input parameters:							*
 *	**uargs 	char		List of layer arguments		*
 *	nargs		int		Number of arguments		*
 *	*glvl		char		User level specification	*
 *									*
 * Output parameters:							*
 *	*nlev		int		Number of levels/layers		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -3 = mixed function		*
 *					 -4 = operator not recognized	*
 *					 -7 = multiple levels required	*
 *					-11 = colon is not allowed	*
 **									*
 * Log:									*
 * T. Lee/SAIC		03/05						*
 * T. Lee/SAIC		04/05						*
 * S. Gilbert/NCEP	12/05	Translation from Fortran     		*
 ************************************************************************/
{
        const int       one = 1;
        int             nval, lvcord, itype, nlvl;
	char		first[9], last[9], inc[9], gtim1[20], gtim2[20];
        char            ingdtm[41], tfirst[21], tlast[21];
	char 		clev [LLMXLV][9], *pclev[LLMXLV];
	float		rdum, rlev[LLMXLV], slev[LLMXLV];
        float           rst, rend, skip, d1, d2, ri;
	/*int		lev[2][LLMXLV];*/
	int		lev1[LLMXLV], lev2[LLMXLV];
	float		rman[NMAN]={ 1000., 925., 850., 700., 500., 400., 
        		             300., 250., 200., 150., 100. };
	int		lflag, idex, i, ly, ln, ilev, ier;
/*----------------------------------------------------------------------*/
	*iret = 0;
        cst_nocc ( glvl, ':', 1, 1, &idex, iret );
	if ( *iret == 0 )  {
	    *iret = -11;
	    return;
	}

        /*
         *	Scan each element of uargs looking for two-level layer based 
         *	diagnostic functions.  If arguments contain both single level 
         *	and layer diagnostic functions, return with an error.
         */
	ly = 0;
	ln = 0;
	for ( i = 0; i < nargs; i++ ) {
	    dg_qlyr ( uargs [ i ], &lflag, &ier );
	    if  ( lflag != 0 ) 
		ly = ly + 1;
	    else
		ln = ln + 1;
	}

	if ( ( ly * ln ) > 0 ) {
	    *iret = -3;
	    return;
	}

        nval = 1;
        dg_iget ("LVCORD", &nval, &lvcord, iret );

        /*
         *	Case I: GLEVEL .eq. "MAN" or GLEVEL is user specified levels.
         */
        for ( i=0; i<LLMXLV; i++ ) pclev[i] = clev[i];
	cst_clst ( glvl, ';', "\0", LLMXLV, 9, pclev, &ilev, &ier );
	if ( ( strcmp(glvl,"MAN") == 0 ) || ( ilev > 1 ) )  {
	    if ( strcmp(glvl,"MAN") == 0 )  {
		if  ( lvcord != 1 )  {
		    *nlev = 0;
		    *iret = -4;
		    return;
	        }
		*nlev = NMAN;
	        dl_setl ( lflag, rman, nlev, iret );
            }
	    else {
		*nlev = ilev;
		for ( i = 0; i < *nlev; i++ ) {
		    cst_crnm ( clev [ i ], &rdum, &ier );
		    rlev [ i ] = rdum;
		}
		lv_sort ( &lvcord, nlev, rlev, iret );
		dl_setl ( lflag, rlev, nlev, iret );
            }
	    return;
	}

	cst_rang ( glvl, first, last, inc, &itype, &ier );
	cst_crnm ( first, &rst, &ier );
	cst_crnm ( last, &rend, &ier );
	cst_crnm ( inc,  &skip, &ier );

        /*
         *	Case II: GLEVEL is in the form of First-Last-Increment.
         */ 
	*nlev = 0;
	if ( itype == 2 ) {
	    if ( rend >= rst ) 
	        for ( ri = rst; ri <= rend; ri += skip ) {
	    	    rlev [ *nlev ] = ri;
		    *nlev = *nlev + 1;
	        }
            else
	        for ( ri = rst; ri >= rend; ri -= skip ) {
	    	    rlev [ *nlev ] = ri;
		    *nlev = *nlev + 1;
	        }

            /*
             *	    Set the levels/layers.
             */
	    lv_sort ( &lvcord, nlev, rlev, &ier );
	    dl_setl ( lflag, rlev, nlev, iret );
        }
	else {

            /*
             *	    Get all of the levels available for the given vertical 
             *	    coordinate.
             */
            dg_cget ( "INGDTM", ingdtm, iret );
            dg_cget ( "TFIRST_1", tfirst, iret );
            dg_cget ( "TLAST_1", tlast, iret );

	    grc_gtim ( ingdtm, tfirst, tlast, gtim1, gtim2, iret );
            nval = LLMXLV;
	    dgc_glev ( &one, gtim1, gtim2, &lvcord, &nval, lev1, lev2, &nlvl,
	               iret );

            /*
             *	    Case III: GLEVEL is in the form of First-Last.
             */
	    if ( itype == 1 )  {
		for ( i = 0; i < nlvl; i++ ) {
		    /*d1 = rst  - (float) lev [ 0 ][ i ];*/
		    /*d2 = rend - (float) lev [ 0 ][ i ];*/
		    d1 = rst  - (float) lev1 [ i ];
		    d2 = rend - (float) lev1 [ i ];
		    if ( ( d1 * d2 ) <= 0. ) {
			/*slev [ *nlev ] = (float) lev [ 0 ][ i ];*/
			slev [ *nlev ] = (float) lev1 [ i ];
			*nlev = *nlev + 1;
		    }
		}
	        lv_sort ( &lvcord, nlev, slev, &ier );
    	        dl_setl ( lflag, slev, nlev, iret );
            }
            /*
             *	      Case IV: GLEVEL .eq. ALL.
             */
	    else if ( strcmp(glvl,"ALL") == 0 )  {
		*nlev = nlvl;
		for ( i = 0; i < *nlev; i++ ) 
		    rlev [ i ] = lev1 [ i ];
		    /*rlev [ i ] = lev [ 0 ][ i ];*/
	        lv_sort ( &lvcord, nlev, rlev, &ier );
    	        dl_setl ( lflag, rlev, nlev, iret );
	    }
	    else if ( ilev == 1 )
		*iret = -7;
	    else
		*iret = -10;
	    
	}

	return;
}
