#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_rshx ( FILE *fp, int rec, shx_record *shxrec, int *iret )
/************************************************************************
 * shp_rshx                                                             *
 *                                                                      *
 * This function reads each record from the index file.        		*
 *                                                                      *
 * shp_rshx ( fp, rec, shxrec, iret )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *      *fp             FILE            Index file pointer              *
 *	rec		int		The record index 		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *shxrec         shx_record      One record in the index file.   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         12/03   	Initial coding                  *
 ***********************************************************************/
{
    shx_recmap recmap;
    long offset;
    int nbin, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Position to the beginning of the record.
     */
    offset = SHX_HEADER_LEN + rec * SHX_RECORD_LEN;
    cfl_seek ( fp, offset, SEEK_SET, &ier );

    /*
     * Map one record to the record structure, and extract
     * corresponding fields.
     */
    cfl_read ( fp, SHX_RECORD_LEN, (unsigned char *)&recmap, 
               &nbin, &ier );
    shxrec->offs = (int)shp_get_blong ( recmap.rec_offs );
    shxrec->lens = (int)shp_get_blong ( recmap.rec_lens );
}
