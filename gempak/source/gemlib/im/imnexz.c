#include "geminc.h"
#include "gemprm.h"

#include "zlib.h"

#define CHECK_ERR(err, msg) { \
    if (err != Z_OK) { \
        printf("%s error: %d\n", msg, err); \
    } \
}

#define	MAX_BLOCK	5000

/************************************************************************
 * imnexz.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

void im_nexz ( char filnam[], int *nline, int *itype, int *ioff, int *iret )
/************************************************************************
 * im_nexz								*
 *									*
 * This routine will look for NEXRAD products in NIDS format with	*
 * headers before the start of the NIDS header. The number of lines to	*
 * expect before the start of product will be 4 is the product has a 	*
 * FOS style transmission header, or 2 if the product is preceeded by	*
 * the WMO and PIL lines.						*
 *									*
 * Both compressed and uncompressed NIDS products will be accepted.	*
 * If the product does not appear to be an uncompressed NIDS product,	*
 * then it will be tested as a zlib compressed product.			*
 *									*
 * On return, the image type will be set to IFNIDS or IFNEXZ if 	*
 * successful and the offset to the first byte past the WMO/PIL header. *
 *									*
 * This subroutine opens a zlib compressed NEXRAD product and sends	*
 * the inflated product to the IM_NIHD2 for reading the header.		*
 * Only the first 39 words of the header are used (156 bytes) by the 	*
 * NIDS header routine, so we only need to inflate the zlib chunks until*
 * we have this many bytes (plus the CCB, WMO and PIL portion of the 	*
 * product. Currently, on NOAAPORT, the largest NWSTG product chunk 	*
 * is 4000 bytes, so that is the largest array size we    		*
 * need to inflate enough of the product to read the header.		*
 *                                                                      *
 * im_nexz ( filnam, nline, itype, ioff, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      filnam[]        char            Name of image file              *
 *	*nline		int		Number of header lines expected	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*itype		int		Type of image found		*
 *	*ioff		int		Offset to start of zlib info	*
 *      *iret		int		Return value			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Chiswell/Unidata	11/00	Created					*
 * S. Jacobs/NCEP	12/00	Removed check for "5" after "SDUS"	*
 * X. Guo/CWS           04/10   Added code to handle 94 product with FOS*
 *                              or WMO header                           *
 * X. Guo/CWS		05/10   Added 99, 134 and 135 products          *
 * M. James/Unidata	04/15   Added TDWR fix                          *
 ***********************************************************************/
{
        FILE    	*fptr;
        char    	*defdir=NULL, newfil[160];
        int     	ierr;
        long    	flen;

	int		nbytes, err, i, j, m;
	int		lenprd, negflg, istart, ival1, ival2;
	unsigned char 	b1, b2;
	unsigned char	*barr=NULL;
	unsigned char 	*boff;

	/*
	 * ZLIB decompression variables.
	 */
	z_stream 	d_stream;
	uLong 		ccblen, kk, lenhdr, lentot, lenout;
	uLong 		uncomprLen = MAX_BLOCK;
	Byte 		uncompr[MAX_BLOCK];

/*---------------------------------------------------------------------*/

	*iret = -1;

        /*
         * Obtain the record length of the input file.
         */
        cfl_inqr ( filnam, defdir, &flen, newfil, &ierr );

        /*
         * Open the input file. The file has already been opened
	 * once, so assume this will work.
         */
        fptr = cfl_ropn ( filnam, defdir, &ierr );
	
        /*
         *  Read the file.
         */
	barr = (unsigned char *) malloc ( flen * sizeof(char) );
        cfl_read ( fptr, flen, barr, &nbytes, &ierr );

        /*
         *  Close the file.
         */
        cfl_clos ( fptr, &ierr );

	if  ( nbytes == flen )  {

	    /*
	     * Find the WMO id in the working array.
	     */
	    i = 0;
	    j = *nline;
	    while  ( ( i < (nbytes - 1) ) && ( j > 0 ) )  {
	    	if  ( ( j == 2 ) &&
		      ( ( i == 0 ) || ( barr[i-1] == '\n' ) ) )  {
		    if  ( ( barr[i]   != 'S' ) ||
		          ( barr[i+1] != 'D' ) ||
		          ( barr[i+2] != 'U' ) ||
		          ( barr[i+3] != 'S' ) )  {
			free ( barr );
			return;
		    }
		}
		if  ( ( barr[i] == '\r' ) && ( barr[i+1] == '\n' ) )  {
		    j -= 1;
		    i += 2;
		}
		else {
		    i += 1;
		}
	    }

	    if  ( j == 0 )  {
		negflg = 0;
		j = 2;

		istart = i;
		mv_btoi ( barr, &istart, &j, &negflg, &ival1, &ierr );

		istart = i + 30;
		mv_btoi ( barr, &istart, &j, &negflg, &ival2, &ierr );

		if  ( ( ival1 == ival2 ) &&
		      ( ival1 >= 16 ) && ( ival1 <= 187 ) )  {
                    if ( ( (ival1 >= 94) && (ival1 <= 187) && 
                           (ival1 != 171) && (ival1 != 169) ) || 
                          (ival1 == 32) ) {
                        //im_nexbzwmo (filnam,barr+i,nbytes-i,itype,ioff,iret);
			im_nzhd ( filnam, barr+i, &i, iret, strlen(filnam) );
                        if  ( *iret == 0 )  {
                            *itype = IFHINIDS;
                            *ioff  = i;
                        }
                    }
                    else {
		        /*
		         * This product is not compressed. Check for a 
		         * valid NIDS product header.
		         */
		        istart = i + 12;
		        mv_btoi ( barr, &istart, &j, &negflg, &ival1,
		    	      &ierr );

		        istart = i + 18;
		        b1 = barr[istart];
		        b2 = barr[istart+1];
		        if  ( ( ival1 < 10000 ) &&
			      ( b1 == 255 ) &&
			      ( b2 == 255 ) )  {

			    /*
			     * Get the header information and save to the
			     * common block.
			     */ 
			    im_nzhd ( filnam, barr+i, &i, iret, strlen(filnam) );
			    if  ( *iret == 0 )  {
			        *itype = IFNIDS;
			        *ioff  = i;
			    }
                         }
                    }
		}
		else {

		    /*
		     * This product may be compressed.
		     */
		    lentot = 0;
		    lenout = 0;
		    boff = barr + i;

		    /*
		     * Continue processing while the total length is
		     * less than both the number of bytes for the
		     * product and the length of the headers.
		     *
		     * lenprd is the number of bytes minus the offset to
		     *        the data minus the 4 for the ending
		     *	      control characters (cr cr nl ETX)
		     * lenhdr is the size of the product header plus the 
		     *        size of the internal header
		     */
		    lenprd = nbytes - i - 4;
		    lenhdr = 156 + 128;

		    while  ( ( (int)lentot < lenprd ) &&
			     ( lentot < lenhdr ) )  {

			d_stream.zalloc = (alloc_func) 0;
			d_stream.zfree  = (free_func) 0;
			d_stream.opaque = (voidpf) 0;

			err = inflateInit ( &d_stream );
			CHECK_ERR ( err, "inflateInit" );

			d_stream.next_in   = (Byte *)(boff + lentot);
			d_stream.avail_in  = nbytes - i - lentot;
			d_stream.next_out  = uncompr + lenout;
			d_stream.avail_out = (uInt)uncomprLen - lenout;

			for  (;;)  {
			    err = inflate ( &d_stream, Z_NO_FLUSH );

			    if  ( err == Z_STREAM_END )  break;

			    CHECK_ERR ( err, "large inflate" );
			    free ( barr );
			    return;
			}

			lentot += d_stream.total_in;
			lenout += d_stream.total_out;
		    }

		    err = inflateEnd ( &d_stream );
		    CHECK_ERR ( err, "inflateEnd" );

		    /*
		     * Strip off CCB, WMO and PIL inside compressed
		     * product.
		     */
		    b1 = uncompr[0];
		    b2 = uncompr[1];

		    ccblen = 2 * ( ( ( b1 & 63 ) << 8 ) + b2 );

		    kk = ccblen;
		    for ( m = 0 ; m < 2 ; m++ )  {
			while ( ( kk < lenout ) &&
				( uncompr[kk] != '\n' ) )  {
			    kk++;
			}
			kk++;
		    }

		    /*
		     * Get the header information and save to the
		     * common block.
		     */
		    im_nzhd ( filnam, uncompr+kk, &i, iret, strlen(filnam) );
		    if  ( *iret == 0 )  {
		    	*itype = IFNEXZ;
			*ioff  = 0;
		    }

		}
	    }
	}

	/*
	 * Free the input file array.
	 */
	free ( barr );

}
