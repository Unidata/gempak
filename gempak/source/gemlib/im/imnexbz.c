#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"

#include <bzlib.h>

/************************************************************************
 * imnexbz.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

void im_nexbz ( char filnam[], int *itype, int *ioff, int *iret )
/************************************************************************
 * im_nexbz                                                             *
 *                                                                      *
 * This subroutine opens a bzlib compressed NEXRAD product and sends    *
 * the inflated product to the IM_NIHD2 for reading the header.         *
 *                                                                      *
 * im_nexbz ( filnam, nline, itype, ioff, iret )                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      filnam[]        char            Name of image file              *
 *                                                                      *
 * Output parameters:                                                   *
 *      *itype          int             Type of image found             *
 *      *ioff           int             Offset to start of bzlib info   *
 *      *iret           int             Return value                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS           04/10   Created                                 *
 * X. Guo/CWS		04/10   Checked product Block Divider and see if*
 *                              the data is compressed                  *
 * X. Guo/CWS           05/10   Used compression method to check if     *
 *                              the product data is compressed          *
 ***********************************************************************/
{
        FILE    	*fptr;
        char    	*defdir=NULL, newfil[160];
        int     	ierr;
        long    	flen;

	int		nbytes, err, i;
	unsigned char	*barr=NULL,*p;
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
        cfl_inqr ( filnam, defdir, &flen, newfil, &ierr );
        /*
         * Open the input file. The file has already been opened
	 * once, so assume this will work.
         */
        fptr = cfl_ropn ( filnam, defdir, &ierr );
        if ( fptr == NULL ) {
            return;
	}
	
        /*
         *  Read the file.
         */
	barr = (unsigned char *) malloc ( flen * sizeof(char) );
        if ( barr == NULL ) {
            cfl_clos ( fptr, &ierr );
            return;
        }
        cfl_read ( fptr, flen, barr, &nbytes, &ierr );

        /*
         *  Close the file.
         */
        cfl_clos ( fptr, &ierr );
	if  ( nbytes == flen )  {
            p = barr;
            compress = (p[100] <<8 ) + p[101];
            memcpy (uncompr, p, RAD_IMG_UNCOMPRESS_LEN);
            p += RAD_IMG_COMPRESS_POS;
            if ( compress == 1 ) {
                destLen = MAX_UNCOMPRESS_BLOCK-RAD_IMG_UNCOMPRESS_LEN;
                err = BZ2_bzBuffToBuffDecompress ((char *)&uncompr[RAD_IMG_COMPRESS_POS],&destLen,(char *)p,flen,0,0);
                if ( err != BZ_OK ) {
		    printf ( "BZ2_bzBuffToBuffDecompresr return error(%d)\n",err );
                    free (barr);
                    return;
	        }
            }
            else {
                memcpy (&uncompr[RAD_IMG_COMPRESS_POS],p,flen-RAD_IMG_COMPRESS_POS);
            }
            	/*
		 * Get the header information and save to the
		 * common block.
		 */
            i = RAD_IMG_COMPRESS_POS;
	    im_nzhd ( filnam, uncompr, &i, iret, strlen(filnam) );
            if  ( *iret == 0 )  {
		*itype = IFHINIDS;
		*ioff  = 0;
	    }
	}

	/*
	 * Free the input file array.
	 */
	free ( barr );
}
