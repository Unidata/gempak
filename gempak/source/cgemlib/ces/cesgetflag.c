#include "geminc.h"
#include "gemprm.h"
#include "cescmn.h"


Boolean ces_getflag ( int subtyp, VG_DBStruct *el, int *iret )
/************************************************************************
 * ces_getflag								*
 *									*
 * This function gets LAYER flag for a specific type of element. The    *
 * passed in element is also queried to determine what type of element  *
 * to retrieve flag for.						*
 *									*
 * ces_getflag ( subtyp, el, iret )					*
 *									*
 * Input parameters:							*
 *	subtyp		int		Element subtype			*
 *									*
 * Input/Output parameters:						*
 *	*el		VG_DBStruct	Element as mask and returned	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -2 = Unable to get settings	*
 * Return parameter:							*
 *	ces_getflag	Boolean		LAYER flag(TRUE or FALSE)	*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	03/03   initial coding				*
 ***********************************************************************/
{
    char	logstr[10], grp[4];
    int         loglev, index, ier1;
/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    /* 
     * Retrieve the index to this type of element in the settings
     * array by making a call to ces_getinx.
     */
    ces_getinx(el, subtyp, &index, iret);
    if (*iret == -2)
    {
        loglev = 2;
        strcpy(grp, "CES");
	sprintf(logstr, "%i", subtyp);
        er_lmsg ( &loglev, grp, iret, logstr, &ier1, strlen(grp),
                        strlen(logstr) );
        return (FALSE);
    }

    /* 
     * Using the index, retrieve the flag info for the element from
     * the settings array.
     */
    return ( set[index].cds_or_ces.layer_flag );

}
