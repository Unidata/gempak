#include "gbcmn.h"
#include "gb2def.h"

void gb2_open ( char *filnam, int *lenfil, Gribmsg *cmsg, int *iret )
/************************************************************************
 * gb2_open                                                             *
 *                                                                      *
 * This function will initialize a gribmsg structure and then open a    *
 * GRIB2 file read only access.                                         *
 *									*
 * gb2_open ( filnam, lenfil, cmsg, iret )			        *
 *									*
 * Input parameters:							*
 *	*filnam		char		GRIB file name			*
 *	*lenfil		int		length of file name		*
 *                                                                      *
 * Output parameters:							*
 *      *cmsg        struct gribmsg     current GRIB field              *
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Gilbert/NCO           11/2004					*
 ***********************************************************************/
{
        int idxfilelen;
        char idxnull[1];
/*---------------------------------------------------------------------*/

	*iret = 0;
	idxnull[0] = CHNULL;
        idxfilelen=0;

	/*
	 *	Initialize gribmsg structure
	 */
        cmsg->cgrib2=0;
        cmsg->mlength=0;
        cmsg->gfld=0;
        cmsg->field_tot=0;

	/*
	 *	Open GRIB file
	 */
        gb_open( filnam, lenfil, idxnull, &idxfilelen, iret);

}
