#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"
#include "proto_xw.h"


void crnowr ( char *imgnam, int *iret )
/************************************************************************
 * crnowr								*
 *									*
 * This subroutine reads the image data from a Copyrighted WSI NOWrad   *
 * format file. The WSI Corporation format is proprietary and not for   *
 * redistribution. No part of this code may be redistributed.           *
 *									*
 * NOWRAD products use a Run Length Encoding scheme using 1, 2 or 3	*
 * bytes to encode color values and the run. The high nibble of the 1st	*
 * byte is a flag:							*
 *									*
 * 0xF - flag byte to change the high nibble of the color. Used for 	*
 *	 changing the color > 15 for graphic annotations (ignored)	*
 * 0xE - 2 byte RLE for runs of 14-256. Color is in lower nibble (0-15) *
 *	 of flag byte, run is second byte				*
 * 0xD - 3 byte RLE for runs of 257-65536. Color is in lower nibble of	*
 *	 first byte (0-15), run is in next two bytes.			*
 * anything else - 1 byte RLE, for runs of 1-13 pixels. High nibble is	*
 *	 color, low nibble is run.					*
 *									*
 * crnowr ( imgnam, iret )						*
 *									*
 * Input parameters:							*
 *	*imgnam		char		Name of image file		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 *					G_NIMGFL = cannot open/read img	*
 *					G_NMEMRY = Memory alloc failure	*
 **									*
 * Log:									*
 * J. Cowie/COMET	 4/95						*
 * J. Cowie/COMET	 5/95	Modified to work with NAWIPS 5.2.1	*
 * J. Cowie/COMET	 9/95	Use stat() to get file size		*
 * S. Jacobs/NCEP	 1/97	Copied from XRNOWR			*
 * J. Cowie/COMET	 1/97	Changed common variable names		*
 * J. Cowie/COMET	12/97	Added cfl_clos if error on cfl_seek	*
 ***********************************************************************/
{

	FILE		*fp;
	char		defdir[12];
	struct stat	fbuf;
	long		lofset;
	unsigned char	*rwdptr, *endptr, tmp[2];
	unsigned int	drun, dcolr, ecode;
	unsigned short	nline;
	int		iend, i, nel, startp, endp, istart,
			nbytes, negflg, ivalue, nbin, ier;

/*---------------------------------------------------------------------*/
	
	*iret = G_NORMAL;
	
/*
 *	Open the file and seek to data offset.
 */
	defdir[0] = CHNULL;
	fp = cfl_ropn ( imgnam, defdir, &ier );
	if  ( ier != 0 )  {
	    *iret = G_NIMGFL;
	    return;
	}

/*
 *	If the data length is undefined, stat() the file to get
 *	the raw image size. (Raw image size (in bytes) is not 
 *	included in the product header, unfortunately)
 */	  
	if  ( imldat == IMISSD )  {
	    stat ( imgnam, &fbuf );	    
 	    imldat = fbuf.st_size - imdoff;
	}

	lofset = (long) imdoff;
	cfl_seek ( fp, lofset, SEEK_SET, &ier );
	if  ( ier != 0 )  {
	    *iret = G_NIMGFL;
	    cfl_clos ( fp, &ier );
	    return;
	}

/*
 *	Allocate space for the raw data (if necessary)
 */	
	if  ( ( rawData == (unsigned char *)NULL ) || 
	      ( imldat > (int)last_rawsize) )  {
			
	    if  ( rawData != (unsigned char *) NULL )
		free(rawData);

	    rawData = (unsigned char *) calloc ( imldat, 
			sizeof(unsigned char) );

	    if  ( rawData == (unsigned char *) NULL )  {
		*iret = G_NMEMRY;
		return;
	    }
	    last_rawsize = imldat;
	}

/*
 *	Read the raw image data.
 */
	cfl_read ( fp, imldat, rawData, &nbin, &ier );
	cfl_clos ( fp, &ier );

/*
 *	Set pointer to raw data array
 */
	rwdptr = rawData;		
	endptr = rwdptr + imldat;
	iend   = 0;

/*
 *	Loop until we've exhausted the image data
 */	 
	while ( ! iend ) {	

/*
 *	    Get the line number. Need to swap bytes.
 */	     
	    rwdptr += 3;
 	    tmp [1] = *rwdptr++;
 	    tmp [0] = *rwdptr++;
	    istart = 0;
	    nbytes = 2;
	    negflg = G_FALSE;
	    mv_btoi ( tmp, &istart, &nbytes, &negflg, &ivalue, &ier );
	    nline = (unsigned short) ivalue;
 	    nel = 0;
/*
 *	    Loop until another line segment is found (or end of image)
 */	     
	    while ( 1 ) {	
	                        
		ecode = *rwdptr >> 4;
		dcolr = *rwdptr & 0xf;

/*
 *		Determine the RLE scheme - 1, 2, or 3 byte encoding.
 *		Ignore high-nibble color flag, check for flag pattern
 *		as data, swap 2-byte runs.
 */		 		
		switch ( ecode )  {

		    case 0xF:
			if  ( ( dcolr == 0x0 ) &&
			      ( *(rwdptr-1) == 0x00 ) &&
			      ( *(rwdptr+1) == 0x00 ) )  {
			    rwdptr += 2;
			}
			else {
			    rwdptr++;
			}
			drun = 0;
			break;

		    case 0xE:
			drun = *(rwdptr+1) + 1;
			rwdptr += 2;
			break;

		    case 0xD:
			tmp [1] = *(rwdptr+1);
			tmp [0] = *(rwdptr+2);
			istart = 0;
			nbytes = 2;
			negflg = G_FALSE;
			mv_btoi ( tmp, &istart, &nbytes, &negflg,
				  &ivalue, &ier );
			drun = (unsigned int)( ivalue + 1 );
			rwdptr += 3;
			break;

		    default:
			drun = ecode + 1;
			rwdptr++;
			break;
		}

/*
 *		Move this run into the image array
 */ 
		startp = (nline - 1) * imnpix + nel;
		endp = startp + drun;		    
		for ( i = startp; i < endp; i++ )  {
		    imgData [i] = dcolr;
		}
		nel += drun;	            

/*
 *		Check for next line or end of image flags
 */ 
		if  ( ( *rwdptr     == 0x00 ) &&
		      ( *(rwdptr+1) == 0xF0 ) &&
	              ( *(rwdptr+2) == 0x0C ) )  {
		    break;
		}

		if ( (( *rwdptr     == 0x00 ) &&
		     ( *(rwdptr+1) == 0xF0 ) &&
	             ( *(rwdptr+2) == 0x02 )) ||
		     ( rwdptr > endptr ) )  {
	            iend++;
		    break;
		}
	    }
	}	
}
