#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"

#include <bzlib.h>

/************************************************************************
 * imnexbzwmo.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

void im_nexbzwmo ( char filnam[],unsigned char *barr, int len, int *itype, int *ioff, int *iret )
/************************************************************************
 * im_nexbzwmo								*
 *									*
 * This routine will process both compressed and uncompressed NIDS      *
 * products in NIDS format.						*
 *									*
 * On return, the image type will be set to IFNIDS or IFNEXZ if 	*
 * sucessful and the offset to the first byte past the WMO/PIL header.	*
 *									*
 * This subroutine opens a bzlib compressed NEXRAD product and sends	*
 * the inflated product to the IM_NIHD2 for reading the header.		*
 * Only the first 39 words of the header are used (156 bytes) by the 	*
 * NIDS header routine, so we only need to inflate the zlib chunks until*
 * we have this many bytes (plus the CCB, WMO and PIL portion of the 	*
 * product. Currently, on NOAAPORT, the largest NWSTG product chunk 	*
 * is 188000 bytes, so that is the largest array size we    		*
 * need to inflate enough of the product to read the header.		*
 *                                                                      *
 * im_nexbzwmo ( filnam, nline, itype, ioff, iret )	        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      filnam[]        char            Name of image file              *
 *      *barr          unsigned char    Image file buffer               *
 *      len             int             Image buffer length             *
 *                                                                      *
 * Output parameters:                                                   *
 *	*itype		int		Type of image found		*
 *	*ioff		int		Offset to start of zlib info	*
 *      *iret		int		Return value			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo		04/10	Created  				*
 * X. Guo/CWS           05/10   Used compression method to check if     *
 *                              the product data is compressed          *
 ***********************************************************************/
{
	int		err, i;
	unsigned char	*p;
        unsigned int    destLen;
        unsigned short  compress;
	/*
	 * BZLIB decompression variables.
	 */
	unsigned char 	uncompr[MAX_UNCOMPRESS_BLOCK];

/*---------------------------------------------------------------------*/

	
        *iret = -1;
        /*
         * Obtain the record length of the input file.
         */

        p = barr;
        compress = (p[100] <<8 ) + p[101];
        memcpy (uncompr, p, RAD_IMG_UNCOMPRESS_LEN);
        p += RAD_IMG_COMPRESS_POS;
        if ( compress == 1 ) {
            destLen = MAX_UNCOMPRESS_BLOCK-RAD_IMG_UNCOMPRESS_LEN;
            err = BZ2_bzBuffToBuffDecompress ((char *)&uncompr[RAD_IMG_COMPRESS_POS],&destLen,(char *)p,len,0,0);
            if ( err != BZ_OK ) {
                printf ( "BZ2_bzBuffToBuffDecompresr return error(%d)\n",err );
                return;
	    }
        }
        else {
            memcpy ( &uncompr[RAD_IMG_COMPRESS_POS], p , len - RAD_IMG_COMPRESS_POS );
        }
        /*
         * Get the header information and save to the
         * common block.
         */
        i = RAD_IMG_COMPRESS_POS;
	im_nzhd ( filnam, uncompr, &i, iret, strlen(filnam) );
        if  ( *iret == 0 )  {
            *itype = IFHINIDS;
	    *ioff  = i;
	}
}
