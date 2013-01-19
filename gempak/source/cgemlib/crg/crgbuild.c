#include "crgcmn.h"
#include "vgstruct.h"

void crg_build ( char *ifname, int *layer, int *iret )
/************************************************************************
 * crg_build                                                            *
 *                                                                      *
 * This function builds the range records from an input VGF file	* 
 *                                                                      *
 * crg_build ( ifname, layer, iret )                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifname		char		VGF filename			*
 *       layer          int             Layer to process                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					 -1 = error opening VG file     *
 *                                       -2 = error closing VG file     *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC      12/05					*
 ***********************************************************************/
{
int		curpos, ier, more, ne;
VG_DBStruct     el;
FILE           *ifptr;
/*---------------------------------------------------------------------*/

    *iret = 0;

    crg_init ( &ier );
    ne = 0;
    more = G_TRUE;
    curpos = 0;

    /*
     *  Open the VG file.
     */
    ifptr = (FILE *) cfl_ropn(ifname, "", &ier);

    if ( ( ier != 0 ) || ( ifptr == NULL ) ) {
        *iret = -1;
        return;
    }

    /*
     *  Loop through all the elements to set the range records. 
     */

    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
        cvg_rdrecnoc ( ifname, ifptr, curpos, &el, &ier );
        if ( ier < 0 )  {
            more = G_FALSE;
        }
        else  {
            crg_set ( &el, curpos, *layer, &ier );
            /*
             * Free TCA break point/GFA blocks memory
             */
            if ( el.hdr.vg_type == TCA_ELM ) {
                cvg_freeBkpts ( &el );
            }
            else if ( el.hdr.vg_type == GFA_ELM ) {
                cvg_freeElPtr ( &el );
            }
            curpos += el.hdr.recsz;
            ne++;
        }
    }

    /*
     *  Close the VG file.
     */

    cfl_clos ( ifptr, &ier );
    if ( ier != 0 )  *iret = -2;

}
