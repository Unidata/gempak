#include "geminc.h"
#include "gemprm.h"

#include "bzlib.h"

#ifdef UNDERSCORE
#define im_bzsec im_bzsec_
#endif

void im_bzsec ( char *imgfil, int *usiz, int *foff, int *doff, unsigned char *barr,
		int *nret, int *ier )
/************************************************************************
 * im_bzsec								*
 *									*
 **									*
 * Log:									*
 ***********************************************************************/
{
    char newfil[160],*defdir=NULL;
    int ierr, i;
    long flen;

    char *dest=NULL, *buf=NULL;
    unsigned int destLen, sourceLen, hlen;
    int nbytes;
    int bzerror, verbosity=0, small=0;

    FILE *fp;

    *ier = 0;

/*    printf("in bzsec %d %d %d asking for %d\n",*foff,*doff,*usiz,*nret);
*/

/*
 *      Get the file size.
 */
    cfl_inqr ( imgfil, defdir, &flen, newfil, &ierr );

/*
 *      Open the input file.
 */
    fp =  cfl_ropn ( imgfil, defdir, &ierr );
    if  ( ierr != 0 )  {
       *ier = G_NIMGFL;
       return;
    }

/*
 *      Read the file.
 */
    buf = (char *) malloc ( flen * sizeof(char) );
    cfl_read ( fp, flen, (unsigned char *)buf, &nbytes, &ierr );

/*
 *      Close the file.
 */
    cfl_clos ( fp, &ierr );

    destLen = *usiz;

    hlen = *foff + ( 2 * (*doff) );
    dest = (char *)malloc(destLen+hlen);
    sourceLen = flen - hlen;

    nbytes = destLen+hlen;
  
/*    printf("hlen: %d\n",hlen);
    printf("flen: %d\n",flen);
    printf("dest: %d\n",destLen+hlen);
*/
    bzerror = BZ2_bzBuffToBuffDecompress( dest, &nbytes, buf+hlen, sourceLen, small, verbosity);

    if ( bzerror == BZ_OK ) {
        if ( *nret != 0 )
	   memcpy(barr,dest,*nret);
        else {
	   memcpy(barr,dest, nbytes);
           *nret = nbytes;
        }
    }
    else
       *ier = bzerror;

    free(buf);
    free(dest);
}
