#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_match ( const int vg_class, const int vg_type,
				const int subtyp, int *indx, int *iret )
/************************************************************************
 * cds_match								*
 *									*
 * This function determines which element attribute settings index	*
 * matches the requested element.					*
 *									*
 * cds_match ( vg_class, vg_type, subtyp, indx, iret )			*
 *									*
 * Input and output parameters:						*
 *	vg_class	const int	Vector Graphics file class	*
 *	vg_type		const int	Vector Graphics file type	*
 *	subtyp		const int	Sub type of vg_type		*
 *									*
 * Output parameters:							*
 *	*indx		int		Index in cdsUattr		*
 *	*iret		int		Return code			*
 *					-2 = Failure to return index	* 
 **									*
 * Log:									*
 * T. Piper/SAIC	12/05	Created					*
 ***********************************************************************/
{
    int		found, sub_type;
/*---------------------------------------------------------------------*/

    found = 0;
    *indx = 0;

    if (vg_type == FRONT_ELM && subtyp != DEFLTSET)
	sub_type = (subtyp / 100) * 100;
    else 
	sub_type = subtyp;

/*
 * Search through the user settings array and find the settings that
 * have the same vector graphic type, class, and subtyp as the
 * inbound element.
 */
    while ( (*indx < numUset ) && (found == 0) ) {
	if  ( (cdsUattr[*indx].vg_type ==vg_type) &&
	    (cdsUattr[*indx].vg_class == vg_class) &&
	    (cdsUattr[*indx].subtyp == sub_type)  ) {
	    found = 1;
	}
	else {
	    (*indx)++;
	}
    }

    if (found == 0) {
	*iret = -2;
	*indx = -1;
    }
    else {
	*iret = 0;
   }
}
