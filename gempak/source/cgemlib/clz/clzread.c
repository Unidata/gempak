#include "geminc.h"
#include "gemprm.h"

#include "zlib.h"


#define CHECK_ERR(err, msg) { \
    if (err != Z_OK) { \
        printf("%s error: %d\n", msg, err); \
    } \
}

/************************************************************************
 * clzead.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

void clz_read ( FILE *fp, int nblocks, long zbytes, int imgsz, unsigned char *data,
		int *ibin, int *ibout, int *iret )
/************************************************************************
 * crnexz								*
 *									*
 * This subroutine reads data from a zlib format file.			*
 *									*
 * clz_read ( fp, zbytes, obytes, data, iret )				*
 *									*
 * Input parameters:							*
 *	filnam[]	char		Name of image file		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 *					G_NIMGFL = cannot open/read img	*
 *					G_NMEMRY = memory alloc failure	*
 **									*
 * Log:									*
 * S. Chiswell/Unidata	11/00	Created from crnids.c			*
 * S. Chiswell/Unidata	12/00	Added realloc check			*
 * S. Chiswell/Unidata	 3/01	Fix inflateEnd memory leak		*
 * S. Chiswell/Unidata	 4/01	Check for bad radial lengths		*
 * S. Chiswell/Unidata	 6/02	Zero out uncompr memory			*
 * S. Chiswell/Unidata	 9/02	Fix inflateEnd memory leak (redux)	*
 * S. Chiswell/Unidata	10/02	Add check for end of file uncompressed	*
 ***********************************************************************/
{


        int     	nbytes, err, ierr;
        char    	*barr=NULL;
        char 		*boff;

	int		i, icnt=0;

	/*
	 * ZLIB decompression variables
	 */
        z_stream 	d_stream;
        uLong 		lentot, lenout;
	static uLong 	uncomprLen = 0;
	static Byte 	*uncompr = NULL;

/*---------------------------------------------------------------------*/
	
	*iret = G_NORMAL;
	*ibin = 0;
	*ibout = 0;

	memset ( data, 0, imgsz );

	uncomprLen = imgsz;
	uncompr = (Byte *)data;

/*
 *	Read the file.
 */
        barr = (char *) malloc ( zbytes * sizeof(char) );
        cfl_read ( fp, zbytes, (unsigned char *)barr, &nbytes, &ierr );


        if  ( nbytes == zbytes )  {
	    lentot = 0;
	    lenout = 0;
	    boff = barr;

/*
 *	    Continue processing while the total length is less 
 *	    than the number of bytes for the product.
 */
	    while ( ( lentot < nbytes ) && ( lenout < imgsz ) &&
		    ( ( icnt < nblocks ) || (nblocks <= 0 ) ) ) {

		d_stream.zalloc = (alloc_func) 0;
		d_stream.zfree  = (free_func) 0;
		d_stream.opaque = (voidpf) 0;
		d_stream.next_in   = (Byte *)(boff + lentot);
		d_stream.avail_in  = nbytes - lentot;

		err = inflateInit ( &d_stream );
		CHECK_ERR ( err, "inflateInit" );

		d_stream.next_out  = uncompr + lenout;
		d_stream.avail_out = (uInt)uncomprLen - lenout;

/*
 *		If we have reached end of zlib portion, exit out of loop.
 *		There could be transmission stuff trailing the product
 *		that is uncompressed. Oct 10, 2002 found that some products have
 *		the last few radials uncompressed (apparent encoding bug).
 */
                if ( ( (d_stream.next_in[0] & 0xf) != Z_DEFLATED ) ||
                     ( ( ( (d_stream.next_in[0] << 8) +
		     	    d_stream.next_in[1] ) % 31 ) != 0 ) )  {
		    err = inflateEnd ( &d_stream );

		    /* just copy over the last few bytes */
		    if ( (nbytes - lentot) > (uncomprLen - lenout ) )
		       memcpy(uncompr+lenout, boff+lentot, uncomprLen - lenout);
		    else
		       memcpy(uncompr+lenout, boff+lentot, nbytes - lentot);
                    break;
		}

		icnt++;

		for  (;;)  {
		    err = inflate(&d_stream, Z_NO_FLUSH);

/*
 *		    If the end of the stream was reached, everything
 *		    is OK, break out of the loop.
 */
		    if  ( err == Z_STREAM_END )  break; 

		    CHECK_ERR ( err, "large inflate" );

		    if ( ( lenout + d_stream.total_out )  != imgsz )
		        {
			err = inflateEnd ( &d_stream );
	        	CHECK_ERR ( err, "inflateEnd" );
		        *iret = G_NMEMRY;
		        free ( barr );
		        return;
		        }
		    else
			break;
		}

		lentot += d_stream.total_in;
		lenout += d_stream.total_out;
	        err = inflateEnd ( &d_stream );
	        CHECK_ERR ( err, "inflateEnd" );
	    }


/*
 *	Free the original data pointer
 */
	free(barr);

	*ibin = lentot;
	*ibout = lenout;
 	}

}
