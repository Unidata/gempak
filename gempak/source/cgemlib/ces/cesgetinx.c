#include "geminc.h"
#include "gemprm.h"
#include "cescmn.h"


void ces_getinx ( VG_DBStruct *el, int subtyp, int *index, int *iret )
/************************************************************************
 * ces_getinx								*
 *									*
 * This function determines which element settings index matches the	*
 * requested element.							*
 *									*
 * ces_getinx ( el, subtyp, index, iret )				*
 *									*
 * Input and output parameters:						*
 *	*el		VG_DBStruct	Element as mask and returned	*
 *	subtyp		int		Element subtyp			*
 *									*
 * Output parameters:							*
 *	*index		int		Index to el location in setgs	*
 *	*iret		int		Return code			*
 *					-2 = Failure to return index	* 
 **									*
 * Log:									*
 * E. Wehner/EAi	 8/97	Created					*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * F.J. Yen/NCEP	 4/98	Renamed from ces_index.  Cleaned up.	*
 ***********************************************************************/
{

    int found;
/*---------------------------------------------------------------------*/

    found = 0;
    *index = 0;

    if (el->hdr.vg_type == FRONT_ELM && subtyp != DEFLTSET)
    {
	subtyp = (subtyp / 100) * 100;
    }

    /*
     * Search through the settings array and find the settings that
     * have the same vector graphic type, class and subtyp as the
     * inbound element.
     */
    while ( (*index < num_set ) && (found == 0) )
    {
	if  ( (set[*index].vg_type == el->hdr.vg_type) &&
	      (set[*index].vg_class == el->hdr.vg_class) &&
	      (set[*index].subtyp == subtyp)  )
	{
	    found = 1;
	}
	else
	{
	    (*index)++;
	}
    }

    if (found == 0)
    {
	/*
	 * Search through the settings array and find the default
	 * settings with matching vg_type and vg_class.
	 */
	*index = 0;
        while ( (*index < num_set ) && (found == 0) )
        {
	    if  ( (set[*index].subtyp == DEFLTSET) &&
		  (set[*index].vg_type == el->hdr.vg_type) &&
	          (set[*index].vg_class == el->hdr.vg_class) )
	    {
	      found = 1;
	    }
	    else
	    {
	      (*index)++;
	    }
        }
    }
    if (found == 0)
    {
	/*
	 * Search through the settings array and find the default
	 * settings with matching vg_class.
	 */
	*index = 0;
        while ( (*index < num_set ) && (found == 0) )
        {
            if  ( (set[*index].vg_type == DEFLTSET) &&
	       	  (set[*index].subtyp == DEFLTSET) &&
                  (set[*index].vg_class == el->hdr.vg_class) )
            {
                found = 1;
            }
            else
            {
                (*index)++;
            }
        }
    }

    if (found == 0)
    {
        *iret = -2;
        *index = -1;
    }
    else
    {
	*iret = 0;
    }
}
