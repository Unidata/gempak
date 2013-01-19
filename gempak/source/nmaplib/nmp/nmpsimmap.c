# include "nmpcmn.h"


void nmp_simmap ( int imgtyp, int lp, Boolean allp, int *iret )
/************************************************************************
 * nmp_simmap                                                           *
 *                                                                      *
 * This function sets the pre-defined map selection which corresponds   *
 * to an image data source selection.  A radar source corresponds to    *
 * the "Custom" map selection, and a satellite source corresponds to    *
 * the "SAT" button.  							*
 *									*
 * This routine is essentially a wrapper for a call to nmp_setmap(),    *
 * and uses the imgtyp to set either the default RAD or SAT setting.    *
 *                                                                      *
 * void nmp_simmap ( imgtyp, lp, allp, iret )                           *
 *                                                                      *
 * Input parameters:                                                    *
 *  imgtyp		int   		SAT_IMG or RAD_IMG   		*
 *  lp			int		loop index 			*
 *  allp		Boolean		all loops options		*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret		int	 	return code			*
 *					   -1  if imgtyp invalid	*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	05/01	initial coding				*
 ***********************************************************************/
{
int 	 ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    switch ( imgtyp ) {

	case SAT_IMG:
	    nmp_setmap(SAT_STR, lp, allp, &ier);
	    *iret = ier;
	    break;

	case RAD_IMG:
	    nmp_setmap(RAD_STR, lp, allp, &ier);
	    *iret = ier;
	    break;

	default:
	    *iret = -1;
	    break;
    }

}

/*=====================================================================*/	
