#include "geminc.h"
#include "gemprm.h"

void ctb_dscatstoi ( char *scatgry, int *scatnum, int *iret )
/************************************************************************
 * ctb_dscatstoi							*
 *									*
 * This function translates a sub-category name into a type number.	*
 *									*
 * ctb_dscatstoi ( scatgry, scatnum, iret )				*
 *									*
 * Input parameters:							*
 *	*scatgry	char		Sub-category name 		*
 *									*
 * Output parameters:							*
 *	*scatnum	int		Sub-category type		*
 *	*iret		int		Return code			*
 *				  	0 - Normal			*
 **									*
 * Log:									*
 * J. Wu/GSC		06/01	initial coding				*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;

    /* 
     *  Translate the sub-category name into a type number.
     */
    if  ( strcmp ( scatgry, "SCAT_SFC" ) == 0 )
        *scatnum = SCAT_SFC;
    else if  ( strcmp ( scatgry, "SCAT_SHP" ) == 0 )
        *scatnum = SCAT_SHP;
    else if  ( strcmp ( scatgry, "SCAT_SFF" ) == 0 )
        *scatnum = SCAT_SFF;
    else if  ( strcmp ( scatgry, "SCAT_FFG" ) == 0 )
        *scatnum = SCAT_FFG;
    else if  ( strcmp ( scatgry, "SCAT_SND" ) == 0 )
        *scatnum = SCAT_SND;
    else if  ( strcmp ( scatgry, "SCAT_SNF" ) == 0 )
        *scatnum = SCAT_SNF;
    else if  ( strcmp ( scatgry, "SCAT_FCT" ) == 0 )
        *scatnum = SCAT_FCT;
    else if  ( strcmp ( scatgry, "SCAT_ANL" ) == 0 )
        *scatnum = SCAT_ANL;
    else
        *scatnum = SCAT_NIL;

}

