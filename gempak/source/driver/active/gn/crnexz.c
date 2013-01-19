#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"
#include "zlib.h"
#include "proto_xw.h"


#define CHECK_ERR(err, msg) { \
    if (err != Z_OK) { \
        printf("%s error: %d\n", msg, err); \
    } \
}

/************************************************************************
 * crnexz.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

void crnexz ( char *imgnam, int *iret )
/************************************************************************
 * crnexz								*
 *									*
 * This subroutine reads the image data from a zlib NIDS format file.	*
 *									*
 * crnexz ( imgnam, iret )						*
 *									*
 * Input parameters:							*
 *	*imgnam		char		Name of image file		*
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
 * S. Chiswell/SRS      1/09    Dynamic allocate radial                 *
 ***********************************************************************/
{

	FILE		*fp;
	char		newfil[160],*defdir=NULL;
	int		ib, ier, nswp, itemp;
	unsigned int	nbend, drun, dcode, ii, iii, jj;
	unsigned short	numr, length, rstang, rdlang, run, ir;
	unsigned char	*rwdptr, *imdptr, *radial;

	int		ierr;
	long		flen;

        int     	nbytes, err, nfail;
        unsigned char 	b1,b2;
        char    	*barr=NULL;
        char 		*boff;

	char		errstr[80];
	static char	errgrp[]="NEXZ";

	/*
	 * ZLIB decompression variables
	 */
        z_stream 	d_stream;
        uLong 		lentot, lenout, lofset, zoff, ccblen;
	static uLong 	uncomprLen = 0;
	static Byte 	*uncompr = NULL;

/*---------------------------------------------------------------------*/
	
	*iret = G_NORMAL;

/*
 *	Get the file size.
 */
	cfl_inqr ( imgnam, defdir, &flen, newfil, &ierr );

/*
 *	Open the input file.
 */
        fp =  cfl_ropn ( imgnam, defdir, &ierr );
        if  ( ierr != 0 )  {
            *iret = G_NIMGFL;
            return;
        }

/*
 *	Prepare the arrays for the uncompressed data.
 */
	if  ( ( uncompr == (Byte *)NULL ) ||
	      ( uncomprLen < (uLong)(imldat+imdoff+128) ) )  {
	    if  ( uncompr != (Byte *)NULL )  {
	    	free ( uncompr );
	    }

/*
 *	    Add 128 to total size for CCB, WMO & PIL
 */
	    uncomprLen = imldat + imdoff + 128;
	    uncompr = (Byte *) calloc ( uncomprLen, sizeof(Byte) ); 

	    if  ( uncompr == (Byte *)NULL )  {
		*iret = G_NMEMRY;
		uncomprLen = 0;
		return;
	    }
	}
	else {
/*
 *	Zero out the uncompr memory previously allocated
 */ 
	   memset (uncompr, 0, uncomprLen);
	}

/*
 *	Read the file.
 */
        barr = (char *) malloc ( flen * sizeof(char) );
        cfl_read ( fp, flen, (unsigned char *)barr, &nbytes, &ierr );

/*
 *	Close the file.
 */
        cfl_clos ( fp, &ierr );


        if  ( nbytes == flen )  {
	    lentot = 0;
	    lenout = 0;
	    boff = barr + imprsz;

/*
 *	    Continue processing while the total length is less 
 *	    than the number of bytes for the product.
 */
	    while  ( lentot < (uLong)(nbytes - imprsz - 4) )  {

		d_stream.zalloc = (alloc_func) 0;
		d_stream.zfree  = (free_func) 0;
		d_stream.opaque = (voidpf) 0;
		d_stream.next_in   = (Byte *)(boff + lentot);
		d_stream.avail_in  = nbytes - imprsz - lentot;

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
		    memcpy(uncompr+lenout, boff+lentot, nbytes - imprsz - lentot);
                    break;
		}

		nfail = 0;
		for  (;;)  {
		    err = inflate(&d_stream, Z_NO_FLUSH);

/*
 *		    If the end of the stream was reached, everything
 *		    is OK, break out of the loop.
 */
		    if  ( err == Z_STREAM_END )  break; 

		    if  ( nfail == 0 )  {

/*
 *			If we failed, try to allocate enough memory
 *			for another block, which might be needed
 *			if the NIDS product has extra sections.
 */
		    	nfail++;

			err = inflateEnd ( &d_stream );
			CHECK_ERR ( err, "inflateEnd" );

			d_stream.zalloc = (alloc_func) 0;
			d_stream.zfree  = (free_func) 0;
			d_stream.opaque = (voidpf) 0;

			uncomprLen += 5000;
			uncompr = (Byte *) realloc (uncompr, uncomprLen);
			d_stream.next_in   = (Byte *)(boff + lentot);
			d_stream.avail_in  = nbytes - imprsz - lentot;

			err = inflateInit ( &d_stream );
			CHECK_ERR ( err, "inflateInit" );

			d_stream.next_out  = uncompr + lenout;
			d_stream.avail_out += 5000;

			continue;

		    }

		    CHECK_ERR ( err, "large inflate" );
		    err = inflateEnd ( &d_stream );
		    *iret = G_NMEMRY;
		    free ( barr );
		    return;
		}

		lentot += d_stream.total_in;
		lenout += d_stream.total_out;
	        err = inflateEnd ( &d_stream );
	        CHECK_ERR ( err, "inflateEnd" );
	    }


/*
 *	    Strip off CCB, WMO and PIL inside compressed product
 */
	    b1 = uncompr[0];
	    b2 = uncompr[1];

	    ccblen = 2 * (((b1 & 63) << 8) + b2);

	    zoff = ccblen;

	    for  ( jj = 0; jj < 2; jj++ )  {
		while ( ( zoff < lenout ) &&
		        ( uncompr[zoff] != '\n') )  {
		    zoff++;
		}
		zoff++;
	    }

	    lofset = (uLong) imdoff + zoff;
	}

/*
 *	Free the original data pointer
 */
	free(barr);
	barr = NULL;

/*
 *	Set the data pointer at image portion of uncompressed product
 */
	rwdptr = uncompr + lofset;

/*
 *	Read the number of radials (radial products) or rows (raster
 *	products) and the product data
 */
        numr = (rwdptr[0] << 8) + rwdptr[1];
	rwdptr+=2;

/*
 *	Set pointers to raw data and image data arrays
 */
	imdptr = imgData;
	
/*
 *	Handle raster products
 */
 	if  ( imrdfl == 0 )  {

/*
 *	    Skip packing descriptor
 */	     
 	    rwdptr += 2;
 	    
/*
 *	    Loop over the number of rows
 */
	    for ( ir = 0; ir < numr; ir++ )  {
	    
		length = *((unsigned short *)rwdptr);

		if  ( imbswp )  {
		    nswp = 1;
		    itemp = length;
		    ier = mv_swp2 ( &nswp, &itemp, &itemp );
		    length = (unsigned short)itemp;
		}

		rwdptr += 2;
		
/*
 *		Run Length decode this line
 */
		iii = 0;
		for ( run = 0; run < length; run++ )  {		
		    drun  = *rwdptr >> 4;
		    dcode = *rwdptr & 0xf;

		    if  ( (int)iii > imnpix )  {
			*iret = G_NIMGFMT;
			return;
		    }

		    for ( ii = 0; ii < drun; ii++ )  {
			*imdptr++ = dcode; 
			iii++;
		    }
		    rwdptr++;
		}
/*
 *		Make sure the line is completed.
 */
		for ( ii = iii; (int)ii < imnpix; ii++ )
			*imdptr++ = 0; 
		
	    }
	}
/*
 *      Handle digital radial products
 */
        else if ( imrdfl == -1 ) {
           printf("got here digital radial crnexz\n");
                *iret = G_NIMGFL;
           return;
        }
	
/*
 *	Handle radial products
 */
 	else {

/*
 *	    Set beam ends to zero for rasterization overflows
 *	    (I don't know if we really need this.)
 */
	    nbend = imnpix / 2;
	    radial = (unsigned char *)malloc((nbend+2)*sizeof(unsigned char));
	    radial [nbend] = 0;
	    radial [nbend+1] = 0;
	  	    
/*
 *	    Loop over the number of radials
 */
	    for ( ir = 0; ir < numr; ir++ ) {		
		
		length = *((unsigned short *) rwdptr);
		rstang = *((unsigned short *) (rwdptr + 2));
		rdlang = *((unsigned short *) (rwdptr + 4));

		if  ( imbswp )  {
		    nswp = 1;

		    itemp = length;
		    ier = mv_swp2 ( &nswp, &itemp, &itemp );
		    length = (unsigned short)itemp;

		    itemp = rstang;
		    ier = mv_swp2 ( &nswp, &itemp, &itemp );
		    rstang = (unsigned short)itemp;

		    itemp = rdlang;
		    ier = mv_swp2 ( &nswp, &itemp, &itemp );
		    rdlang = (unsigned short)itemp;
		}

		rwdptr += 6;

/*
 *		Run Length decode this radial
 */
		ib = 0;
		for ( run = 0; run < (unsigned short)(2*length); run++ )  {
		    if  (rwdptr > uncompr+uncomprLen )  {
			sprintf ( errstr, "bad radial [%d %d]",
				  run,length);
		        ierr = 1;
		        er_wmsg ( errgrp, &ierr, errstr, &ier, 
			          strlen(errgrp), strlen(errstr) );
		        return;
		    }
		    drun  = *rwdptr >> 4;
		    dcode = *rwdptr & 0xf;
		    for ( ii = 0; ii < drun; ii++ )  {
			radial[ib] = dcode;
			ib++;
		    }
		    rwdptr++;
		}
		
/*
 *		Insert this radial in the rasterized image
 */
		crastr ( radial, ib, imnpix,
			 (float)rstang/10., (float)rdlang/10., &ier);

    	    }
	    free ( radial );
 	}

}
