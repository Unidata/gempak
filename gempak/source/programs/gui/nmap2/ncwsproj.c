#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "proto_nmaplib.h"

void ncw_sproj ( const char *source )
/************************************************************************
 * ncw_sproj                                                          	*
 *                                                                      *
 * This function sets the projection information for the computational	*
 * window. The computational window usage may be specified in the 	*
 * 'prefs.tbl' table using the boolean keyword 'COMP_WINDOW'. The	*
 * projection to be employed is specified in 'source', a tagged string.	*
 * For example:								*
 * '<PROJ>str/90;-97;0<GAREA>19.00;-119.00;47.00;-56.00'                *
 * A default projection and geographical area may be specified in the 	*
 * 'prefs.tbl' with 'COMP_PROJ'	and 'COMP_GAREA'.  			*
 * If any of the provided projection information cannot be decoded - 	*
 * either through 'source' or the 'prefs.tbl' table - then an error is	*
 * set and whatever projection is in the main window will be used.	*
 *                                                                      *
 * void ncw_sproj( char *source )                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *  source	const char	projection information string		*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	12/06						*
 ***********************************************************************/
{
int	p, ier, ier1, ier2, mone, idrpfl;
char 	*src, PROJ[32], GAREA[32];
Boolean	qcomp;

/*---------------------------------------------------------------------*/

    ctb_pfbool ( "COMP_WINDOW", &qcomp, &ier );

    if ( qcomp )  {

    G_MALLOC ( src, char, strlen(source)+1, 
	    "ncw_sproj: Error allocating src" );
    strcpy ( src, source );

    p = G_TRUE;

    if ( strcmp ( src, "PREFS" ) != 0 )  {

	/*
	 * If user input is not exactly 'PREFS' (meaning get the projection 
	 * info from 'prefs.tbl'), then try to find 'PROJ' and 'GAREA' 
	 * tags and parse/process that info.
	 */
	cst_gtag (  "PROJ", src, "",  PROJ, &ier1 );
	cst_gtag ( "GAREA", src, "", GAREA, &ier2 );

	if ( ier1 != 0 || ier2 != 0 )  {

	    /*
	     * If tags cannot be found, then process as if 'PREFS' was
	     * specified.
	     */
	    strcpy ( src, "PREFS" );

	}
    }

    if ( strcmp ( src, "PREFS" ) == 0 )  {

	/*
	 * Get the projection specs from 'prefs.tbl'.
	 * If not found, set error and use main window projection.
	 */
	ctb_pfstr ( "COMP_PROJ", PROJ, &ier1 );
	ctb_pfstr ( "COMP_GAREA", GAREA, &ier2 );

	if ( ier1 != 0 || ier2 != 0 )  {
	    p = G_FALSE;
	}
	
    }

    if ( p )  {

	gg_maps ( PROJ, GAREA, "", &idrpfl, &ier, 
		strlen(PROJ), strlen(GAREA), strlen("") );

	if ( ier != 0 )  {
	    p = G_FALSE;
	}

    }

    if ( !p )  {
	ncw_unset ();
	mone = -1;
	er_wmsg ( "NCW", &mone, NULL, &ier, strlen("NCW"), 0 );
	NxmErr_update ();
    }

    G_FREE ( src, char );

    }

}
