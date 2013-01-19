#include "gbcmn.h"
#include "gb2def.h"

void gb2_next( Gribmsg *cmsg, int *ivers , int *iret )
/************************************************************************
 * gb2_next								*
 *									*
 * This function finds and unpacks the next available GRIB2 field.      *
 * If the current GRIB2 message contains another field, the metadata    *
 * associated with that field will be unpacked.                         *
 * If there is not an additional field in the current GRIB2 message,    *
 * a new message is read from the file, and the metadata for the first  *
 * field is unpacked.                                                   *
 *									*
 * gb2_next ( cmsg, ivers, iret )					*
 *									*
 * Output parameters:							*
 *      *cmsg        struct gribmsg     current GRIB field              *
 *	*ivers		int		GRIB version number		*
 *	*iret		int		Return code			*
 *					  3 = Not GRIB version 2        *
 *                                        1 = grid too large            *
 *					-16 = No more GRIB messages in  *
 *                                            file                      *
 *                                      -19 = error reading GRIB file   *
 *					-25 = error unpacking metadata  *
 *                                      -26 = Could not allocate enough *
 *                                            memory to process next    *
 *                                            GRIB2 message             *
 *                                      -28 = Not valid GRIB2 message   *
 **									*
 * Log:									*
 * J. Chou/EAI		 7/93						*
 * S. Jacobs/EAI	 7/93	Clean up				*
 * S. Jacobs/EAI	11/93	Added return of version number		*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * D.W.Plummer/NCEP	 3/96	Change for cfl_ call sequence		*
 * D.W.Plummer/NCEP	 6/96	Reset length variables to zero		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * S. Gilbert        1/04   Modified from gb_next for reading GRIB2 files.  *
 * S. Gilbert/NCEP          03/06    Chngs to remove compiler warnings  *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    int   len,iret2,prev;
    g2int n,numlocal;
    g2int unpack,expand;
    g2int  listsec0[3],listsec1[13];

    *iret = 0;
    iret2 = 0;
    unpack=0;
    expand=0;

    /*
    printf("1 %x \n",cmsg->cgrib2);
    printf("2 %x \n",cmsg->gfld);
    printf("3 %d \n",cmsg->mlength);
    printf("4 %d \n",cmsg->field_tot);
    */

    prev=0;
    if ( cmsg->gfld != 0 ) {
       n=cmsg->gfld->ifldnum;      /*  save current field number  */
       g2_free(cmsg->gfld);        /*  free memory for previous field  */
       cmsg->gfld=0;
       prev=1;
    }

    /*
     *  If previous GRIB2 message exists, and there is another field in that
     *  message, unpack metadata associated w/ that field.
     */

    if ( prev == 1  &&  n < (int)cmsg->field_tot ) {
         n++;
	 *iret=(int)g2_getfld(cmsg->cgrib2,n,unpack,expand,&(cmsg->gfld));
         if ( *iret != 0 ) *iret=-25;
         return;
    }
    else {

        /*
         *  Scan the file and read in the next message and unpack the
         *  metadata for the first field.
         */


        /*
         * first, free memory from previous message, if exists  
         */
        if ( cmsg->cgrib2 != 0 ) free(cmsg->cgrib2); 
	cmsg->cgrib2=0;


        /*
         *  Scan the file for next message
         */
	gb_scan ( gbfile.fptr, infile.fptr, ivers, iret );
	if ( *iret == 0 ) {
	     cmsg->mlength = cursor1 - cursor;
	     len = cmsg->mlength;
             /*
              *  Allocate space for next message
              */
             if ( (cmsg->cgrib2=(unsigned char *)malloc(cmsg->mlength)) != 0 ) {
                 /*
                  *  Read next message
                  */
                 gb2_read(&len, cmsg->cgrib2, iret);
                 if ( *iret != 0 ) return;
             }
             else {     /*   Could not allocate space  */
                 *iret = -26;
                 return;
             }
        }
        else return;        /*  no next message in file  */

        /*
         *  If the new message is GRIB version 2,
         *  Check validity of GRIB2 message and return the number of 
         *  fields contained in the new message.
         */
        if ( *ivers == 2 ) {
            *iret=(int)g2_info(cmsg->cgrib2, listsec0, listsec1, 
                               &(cmsg->field_tot), &numlocal);
            if ( *iret != 0 ) {
                er_wmsg("GB2",iret," ",&iret2,3,1);
                *iret=-28;
                return;
            }
            /*
             *  unpack metadata for first field.
             */
            n=1;
            *iret=(int)g2_getfld(cmsg->cgrib2,n,unpack,expand,&(cmsg->gfld));
            if ( *iret != 0 ) *iret=-25;
        }
        else {
             *iret = 3;
             return;
        }

    }    /* end else block reading new message from file  */

}
