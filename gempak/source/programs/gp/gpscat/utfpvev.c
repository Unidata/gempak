#include "geminc.h"
#include "gemprm.h"

void utf_pvev ( unsigned char *ptr, int *add, int *iret )
/************************************************************************
 * utf_pvev								*
 *									*
 * This function plots VEV vectors (C5) from a UTF file.		*
 *									*
 * utf_pvev ( ptr, add, iret )						*
 * 									*
 * Input parameters:							*
 *	*ptr		unsigned char	Position in buffer		*
 *									*
 * Output parameters:							*
 *	*add		int		Size of record in bytes		*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96	Copied from utf_vev			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 ***********************************************************************/
{
    static int 		xmult [8] = { 0, 1, 1, 1, 0, -1, -1, -1};
    static int 		ymult [8] = { 1, 1, 0, -1, -1, -1, 0, 1};
    int 		ipnt, jpnt, vlen, vdir, bitpos, zt, zf;
    int 		ier, jj, lasdir, dev, ypnt[1000], xpnt[1000], bytadd;
    unsigned int	bits, i, j, k;
    float		rx[1000], ry[1000];
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
**  Initialize vector arrays to zero.
*/
    for ( k = 0; k < 1000; k++ ) {
	xpnt[k] = 0;
	ypnt[k] = 0;
	rx[k]   = 0.0F;
	ry[k]   = 0.0F;
    }

/*
**  Decode the header of the C5 record.
*/
    utf_dvev( ptr, &vdir, &zt, &zf, &vlen, &ipnt, &jpnt, &bits, &bytadd,
	      &ier );
    *add = bytadd;
    ptr += 8;

/*
**  Fill the first element of vector arrays with I, J coordinate.
*/
    lasdir = vdir;
    xpnt[0] = ipnt;
    rx[0] = (float) xpnt[0];
    ypnt[0] = jpnt; 
    ry[0] = (float) ypnt[0];

/*
**  Loop over each word, decoding each bit of data.
*/
    j = 1;
    for ( i = 1; i <= bits; i += 8, ptr++ ) {

    	for ( bitpos = 7; bitpos >= 0; bitpos-- ) {

      	    if ( (i + 7 - bitpos) <= bits ) {

		ipnt = ipnt + xmult [vdir] * vlen;
		jpnt = jpnt + ymult [vdir] * vlen;

		if ( ( ( *ptr >> bitpos ) & 0x01 ) == 1 ) {

	  	    if ( vdir != lasdir ) {
		    	dev = abs(lasdir - vdir);

			if ( vlen > 4 || ( dev > 0 && dev < 8 ) ) {
	      		    xpnt[j] = ipnt;
	      		    ypnt[j] = jpnt;
			    rx[j] = (float) xpnt[j];
			    ry[j] = (float) ypnt[j];
	      		    lasdir = vdir;
			    j++;
	      		}
	    	    }

		    if ( ( bitpos % 2 ) == 1 ) {
	    		vdir -= 1;
	    		if ( vdir < 0 )
	      		    vdir = 7;
	    	    }
	  	    else {
	    		vdir += 1;
	    		if ( vdir > 7 )
	      		    vdir = 0;
	    	    }
	  	}
      	    }
    	}
    }

    if ( ipnt != xpnt[j-1] || jpnt != ypnt[j-1] ) {
    	xpnt[j] = ipnt;
    	ypnt[j] = jpnt;
	rx[j] = (float) xpnt[j];
	ry[j] = (float) ypnt[j];
    	j++;
    }

/*
**  Pass vector arrays to be plotted into GLINE.
*/
    jj = j;
    gline( sys_G, &jj, rx, ry, &ier, strlen(sys_G) );

}
