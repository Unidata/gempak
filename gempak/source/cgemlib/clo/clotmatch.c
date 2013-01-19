#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t	clo;

void clo_tmatch ( char *name, float platx, float plonx, float tol, int *iret )
/************************************************************************
 * clo_tmatch								*
 *									*
 * This function returns the index into the lat/lon arrays of the	*
 * element which is a geographical match to the input lat and lon.	*
 * Attm, this function applies only to standard station tables.		*
 * A tolerance is allowed for acceptable closeness; a tolerance of 	*
 * zero forces an exact match an may be risky since floating point 	*
 * comparisons are being performed.					*
 *									*
 * clo_tmatch  ( name, platx, plonx, tol, iret )			*
 *									*
 * Input parameters:							*
 *	*name		char		Name of location name		*
 *	platx		float		Latitude of point		*
 *	plonx		float		Longitude of point		*
 *	tol		float		Tolerance			*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *				= -1 - Invalid lat or lon		*
 *				= -2 - Unable to locate name		*
 *				= -3 - Invalid type			*
 *				= -4 - Unable to find exact match	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	12/06	Created from clo_tclosest		*
 * D.W.Plummer/NCEP	01/07	Improve error handling			*
 ***********************************************************************/
{
int	which, indx, indx1, indx2, ier;
float	plat, plon;
/*---------------------------------------------------------------------*/
    *iret = 0;
    plat = platx; 
    plon = plonx;

    /*
     *  First check for validity of latitude and longitude
     */
    if ( ERMISS(plat) || ERMISS(plon) || plat > 90.0F || plat < -90.0F )  {
	nhot = 0;
	*iret = -1;
	return;
    }
    else {
        plon = (float)fmod( (double)plonx, 360.0 );
        if ( plon > 180.0F )  plon -= 360.0F;
    }

    which = clo_which ( name );
    if ( which < 0 )  {
	*iret = -2;
	return;
    }

    switch( clo.loc[which].format )  {

	case 0:			/* Standard station table format	*/

    	    clo_sortstn( name, STN_LON, &ier );

	    nhot = 0;
	    if ( npStn != 0 )  {
		indx1 = 0;
		indx2 = npStn-1;
		indx = ( indx1 + indx2 ) / 2;
		while ( indx2-indx1 > 1 )  {
		    if ( lonStn[indx] > plon )  {
			indx1 = indx;
		    }
		    else  {
			indx2 = indx;
		    }
		    indx = ( indx1 + indx2 ) / 2;
		}

		indx = indx1;
		while ( G_DIFFT(lonStn[indx], plon, tol) == G_TRUE )  indx -= 1;
		indx += 1;
		while ( G_DIFFT(lonStn[indx], plon, tol) == G_TRUE )  {
		    if ( G_DIFFT(latStn[indx], plat, tol) == G_TRUE )  {
			hotlist[nhot] = indx;
			nhot = 1;
			break;
		    }
		    indx += 1;
		}
		if ( nhot == 0 )  {
		    *iret = -4;
		}
	    }

	    break;

	default :		/* Anything other format		*/

	    *iret = -3;

	    break;

    }

}
