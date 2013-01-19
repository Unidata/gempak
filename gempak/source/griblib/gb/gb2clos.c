#include "gb2def.h"
#include "gbcmn.h"

void gb2_clos ( Gribmsg *cmsg, int *iret )
/************************************************************************
 * gb2_clos								*
 *									*
 * This function will free any used memory allocated for struct gribmsg *
 * and close a GRIB file                                                *
 *                                                                      *
 * gb2_clos ( cmsg, iret )                             		        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *cmsg        struct gribmsg     current GRIB field              *
 *      *iret		int		Return code                     *
 *                                        -15 = error closing file      *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCO           11/2004                                     *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = 0;


        /*
        if ( cmsg->gfld != 0 )printf("%x\n",cmsg->gfld->idsect);
        if ( cmsg->gfld != 0 )printf("%x\n",cmsg->gfld->local);
        if ( cmsg->gfld != 0 )printf("%x\n",cmsg->gfld->list_opt);
        if ( cmsg->gfld != 0 )printf("%x\n",cmsg->gfld->igdtmpl);
        if ( cmsg->gfld != 0 )printf("%x\n",cmsg->gfld->coord_list);
        if ( cmsg->gfld != 0 )printf("%x\n",cmsg->gfld->idrtmpl);
        if ( cmsg->gfld != 0 )printf("%x\n",cmsg->gfld->bmap);
        if ( cmsg->gfld != 0 )printf("%x\n",cmsg->gfld->fld);
        */
        /*
        **	Free any allocated memory
        */
        if ( cmsg->cgrib2 != 0 ) free(cmsg->cgrib2);
        if ( cmsg->gfld != 0 ) g2_free(cmsg->gfld);
        cmsg->cgrib2=0;
        cmsg->mlength=0;
        cmsg->gfld=0;
        cmsg->field_tot=0;
       

        /*
        **	Close the GRIB file.
        */
        gb_clos ( iret );


}

