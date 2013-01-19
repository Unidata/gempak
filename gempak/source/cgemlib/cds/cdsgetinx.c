#include "geminc.h"
#include "gemprm.h"

#include "cds.h"

void cds_getinx ( VG_DBStruct *el, int *indx, int *iret )
/************************************************************************
 * cds_getinx								*
 *									*
 * This function determines which element attribute settings index	*
 * matches the requested element.					*
 *									*
 * cds_getinx ( el, indx, iret )					*
 *									*
 * Input and output parameters:						*
 *	*el		VG_DBStruct	Element as mask and returned	*
 *									*
 * Output parameters:							*
 *	*indx		int		Index to el location in usetting*
 *	*iret		int		Return code			*
 *					-2 = Failure to return index	* 
 **									*
 * Log:									*
 * F. J. Yen/NCEP	10/99	Based on ces_getinx			*
 * A. Hardy/SAIC	11/01   Renamed cds_subtyp to cvg_subtyp	*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 ***********************************************************************/
{

    int		found;
    int		subtyp;
/*---------------------------------------------------------------------*/

    found = 0;
    *indx = 0;

    /*
     *	Get subtyp of element
     */

    cvg_subtyp (el, &subtyp, iret);

    if ( *iret == 0 ) {

	if (el->hdr.vg_type == FRONT_ELM && subtyp != DEFLTSET) {
	    subtyp = (subtyp / 100) * 100;
	}

	/*
	 * Search through the user settings array and find the settings that
	 * have the same vector graphic type, class and subtyp as the
	 * inbound element.
	 */
	while ( (*indx < numUset ) && (found == 0) ) {
	    if  ( (cdsUattr[*indx].vg_type == el->hdr.vg_type) &&
		  (cdsUattr[*indx].vg_class == el->hdr.vg_class) &&
		  (cdsUattr[*indx].subtyp == subtyp)  ) {
		found = 1;
	    }
	    else {
		(*indx)++;
	    }
	}

	if (found == 0) {
	    /*
	     * Search through the user settings array and find the default
	     * settings with matching vg_type and vg_class.
	     */
	    *indx = 0;
            while ( (*indx < numUset ) && (found == 0) ) {

		if  ( (cdsUattr[*indx].subtyp == DEFLTSET) &&
		      (cdsUattr[*indx].vg_type == el->hdr.vg_type) &&
		      (cdsUattr[*indx].vg_class == el->hdr.vg_class) ) {
		    found = 1;
		}
		else {
		    (*indx)++;
		}
            }
	}
	if (found == 0) {
	    /*
	     * Search through the user settings array and find the default
	     * settings with matching vg_class.
	     */
	    *indx = 0;
            while ( (*indx < numUset ) && (found == 0) )
            {
		if  ( (cdsUattr[*indx].vg_type == DEFLTSET) &&
		      (cdsUattr[*indx].subtyp == DEFLTSET) &&
		      (cdsUattr[*indx].vg_class == el->hdr.vg_class) ) {
               	    found = 1;
		}
		else {
                    (*indx)++;
		}
            }
	}
    }

    if (found == 0)
    {
        *iret = -2;
        *indx = -1;
    }
    else
    {
	*iret = 0;
    }
}
