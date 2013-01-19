#include "geminc.h"
#include "gemprm.h"

void ctb_dcatstoi ( char *catgry, int *catnum, int *iret )
/************************************************************************
 * ctb_dcatstoi								*
 *									*
 * This function translates a category name into a type number.		*
 *									*
 * ctb_dcatstoi ( catgry, catnum, iret )				*
 *									*
 * Input parameters:							*
 *	*catgry		char		Category name 			*
 *									*
 * Output parameters:							* 
 *	*catnum		int		Category type			*
 *	*iret		int		Return code			*
 *					0 - Normal			*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	initial coding				*
 * M. Li/SAIC		03/08	Added CAR_ENS				*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;

    /* 
     *  Translate the category name into a type number.
     */
    if  ( strcmp ( catgry, "CAT_IMG" ) == 0 )
        *catnum = CAT_IMG;
    else if  ( strcmp ( catgry, "CAT_SFC" ) == 0 )
        *catnum = CAT_SFC;
    else if  ( strcmp ( catgry, "CAT_SFF" ) == 0 )
        *catnum = CAT_SFF;
    else if  ( strcmp ( catgry, "CAT_SND" ) == 0 )
        *catnum = CAT_SND;
    else if  ( strcmp ( catgry, "CAT_SNF" ) == 0 )
        *catnum = CAT_SNF;
    else if  ( strcmp ( catgry, "CAT_GRD" ) == 0 )
        *catnum = CAT_GRD;
    else if  ( strcmp ( catgry, "CAT_VGF" ) == 0 )
        *catnum = CAT_VGF;
    else if  ( strcmp ( catgry, "CAT_MSC" ) == 0 )
        *catnum = CAT_MSC;
    else if  ( strcmp ( catgry, "CAT_ENS" ) == 0 )
        *catnum = CAT_ENS;
    else
        *catnum = CAT_NIL;
    
}

