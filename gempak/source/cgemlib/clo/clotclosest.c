#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern	CLO_t	clo;

void clo_tclosest ( char *name, float platx, float plonx, int nclose, 
								int *iret )
/************************************************************************
 * clo_tclosest								*
 *									*
 * This function returns the index into the lat/lon arrays of the	*
 * element which is closest geographically to the point in question.	*
 * Attm, this function applies only to standard station tables.		*
 *									*
 * clo_tclosest  ( name, platx, plonx, nclose, iret )			*
 *									*
 * Input parameters:							*
 *	*name		char		Name of location name		*
 *	platx		float		Latitude of point		*
 *	plonx		float		Longitude of point		*
 *	nclose		int		# of indices to return		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					= -1 - Unable to locate closest	*
 *					= -2 - Unable to locate name	*
 *					= -3 - Invalid type		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 6/97	Created					*
 * D.W.Plummer/NCEP	11/97	Added format parameter			*
 * F. J. Yen/NCEP	 9/98	Added format SFSTN (surface station)	*
 * D.W.Plummer/NCEP	12/98	Change clo_closest calling sequence	*
 * D.W.Plummer/NCEP	 1/99	LOC structure name changes		*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 * D.W.Plummer/NCEP	 4/99	Updated for MARINE and COASTAL formats	*
 * D.W.Plummer/NCEP	12/99	Add checks for lat/lon validity		*
 * D.W.Plummer/NCEP	 1/00	Add checks for returned index validity	*
 * D.W.Plummer/NCEP	 8/00	Update for revised CLO library		*
 * D.W.Plummer/NCEP	 8/00	Improve error chk; mv sort into case	*
 ***********************************************************************/
{
int	i, which, ier;
float	plat, plon;

int	*index;

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

    index = (int *) malloc( (size_t)nclose * sizeof(int) );

    switch( clo.loc[which].format )  {

	case 0:			/* Standard station table format	*/

    	    clo_sortstn( name, STN_LON, &ier );

	    nhot = 0;
	    if ( npStn != 0 )  {
    	        clo_closest ( latStn, lonStn, npStn, 
		              plat, plon, nclose, index, iret );
	        for ( i = 0; i < nclose; i++ )  {
		    if ( index[i] != IMISSD )  {
			hotlist[nhot] = index[i];
			nhot++;
		    }
		}
	    }

	    break;

	default :		/* Anything other format		*/

	    *iret = -3;

	    break;

    }

    free ( index );

}
