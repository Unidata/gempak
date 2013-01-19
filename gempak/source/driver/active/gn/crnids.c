#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"
#include "proto_xw.h"

short nids_short (unsigned char *x)
{
  short ival = (x[0] << 8) | x[1];
  return (ival);
}

int nids_int (unsigned char *x)
{
  int ival = (((((x[0] << 8) | x[1]) << 8) | x[2]) << 8) | x[3];
  return (ival);
}

#ifdef UNDERSCORE
#define im_bzsec im_bzsec_
#endif

void crnids ( char *imgnam, int *iret )
/************************************************************************
 * crnids								*
 *									*
 * This subroutine reads the image data from a raw NIDS format file.	*
 *									*
 * crnids ( imgnam, iret )						*
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
 * J. Cowie/COMET	 3/95						*
 * J. Cowie/COMET	 5/95	Modified to work with NAWIPS 5.2.1	*
 * S. Jacobs/NCEP	 1/97	Copied from XRNIDS			*
 * J. Cowie/COMET	 1/97	Changed common variable names		*
 * J. Whistler/AWC	 4/97	Added check to make sure rasterized run	*
 *				length encoded lines finish to the end	*
 * J. Cowie/COMET	12/97	Added cfl_clos if error on cfl_seek	*
 * S. Jacobs/NCEP	 5/99	Added byte swapping			*
 * S. Jacobs/NCEP	10/00	Added check when data run is too long	*
 * T. Piper/SAIC	11/01	Added check when radial too long	*
 * R. Tian/SAIC		03/03	Dynamic allocate radial			*
 * T. Piper/SAIC	03/08	Added support for 1 km mosaics		*
 * T. Piper/SAIC	07/08	Fix bug introduced on 3/08		*
 ***********************************************************************/
{

	FILE		*fp;
	char		defdir[12];
	long		lofset;
	int		nbin, ier, ires, nswp, itemp;
	unsigned int	drun, dcode, ib, ii, jj, nbend, NSSL;
	unsigned short	ir, numr, length, rstang, rdlang, run, row;
	unsigned char	*rwdptr, *imdptr, *radial;

/*---------------------------------------------------------------------*/
/*
 *  Open the file and seek to data offset.
 */
    defdir[0] = CHNULL;
    fp = cfl_ropn ( imgnam, defdir, &ier );
    if  ( ier != 0 )  {
	*iret = G_NIMGFL;
    return;
    }
    else {
	lofset = (long)imdoff;
	cfl_seek ( fp, lofset, SEEK_SET, &ier );
	if  ( ier != 0 )  {
	    cfl_clos ( fp, &ier );
	    *iret = G_NIMGFL;
	    return;
	}
    }

/*
 *  Allocate space for the raw data (if necessary)
 */
    if  ( ( rawData == (unsigned char *)NULL ) ||
	( imldat > (int)last_rawsize) ) {
		
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
 *	Set pointers to raw data and image data arrays
 */
    imdptr = imgData;
    rwdptr = rawData;

/*
 *      Handle digital radial products separately
 */
    if ( imrdfl == -1 ) {
	int usiz = 2 * imldat+16; /* need to allocate product size plus header, need to fix this */
	int off1 = imdoff+imprsz, off2 = 0; /* use off1 for bytes, off2 for halfwords */
	int nret=0;
	short is, isd, packet_code, layers;
	int layer_length;
	unsigned char *lptr;

	cfl_clos ( fp, &ier );

        /* see if we need to increase rawData size to uncrompress the entire 
 * 	bzipped product into */

        if ( last_rawsize < usiz ) {
	    free(rawData);
	    rawData = (unsigned char *) calloc ( usiz,
				sizeof(unsigned char) );
            rwdptr = rawData;
            last_rawsize = usiz;
        }

	im_bzsec ( imgnam, &usiz, &off1, &off2, rawData, &nret, &ier );

        if ( ier != 0 ) {
   	    /* not a bzip compressed packet */
	    printf("expecting bzip2 compression, not found %d\n", ier);
            *iret = G_NIMGFL;
            return;
        }

        /* verify start of product symbology block */
        is = nids_short(rwdptr); rwdptr += 2; isd = nids_short(rwdptr); rwdptr += 2;
        if ( ( is != -1 ) && ( isd != 1 ) ) {
            printf("unexpected block structure\n");
	    *iret = G_NIMGFL;
	    return;
        }

        /* get number of layers */
        rwdptr += 4;
	layers = nids_short(rwdptr); rwdptr += 2;
        /*printf("layers %d\n",layers);*/

        lptr = rwdptr;
        for(ii=0; ii< layers;ii++) {
           lptr += 2; /* skip over divider -1 */
           layer_length = nids_int(lptr); lptr += 4;
           /*printf("%d layer_length %d\n",ii,layer_length);*/

           /* get packet code */
	   packet_code = nids_short(lptr);
           switch ( packet_code ) {
	       case 16:
                   lptr += 12;
		   numr = nids_short(lptr); lptr+= 2;
	  	   for ( ir = 0; ir < numr; ir++ ) {
		       length = nids_short(lptr);
		       rstang = nids_short(lptr+2);
		       rdlang = nids_short(lptr+4);
		       crastr ( lptr+6, length, imnpix,
                           (float)rstang/10., (float)rdlang/10., &ier);
		       lptr += ( length + 6);
		   }

		   break;
	       case 1:
	       case 8:
	  	   length = nids_short(lptr+2);
		   lptr += ( length + 4);
		   break;
	       default:
		   printf("unhanlded crnids radar packet code %d [layer %d/%d] lptr %d\n",packet_code,ii,layers,lptr);
            }
               
        }
        rwdptr += layer_length; /* not really needed */

        return;
    }
    *iret = G_NORMAL;

/*
 *  Read the number of radials (radial products) or rows (raster
 *  products) and the product data
 */
    cfl_read ( fp, 2, (unsigned char *)&numr, &nbin, &ier );
    cfl_read ( fp, imldat, rawData, &nbin, &ier );
    cfl_clos ( fp, &ier );

    if  ( imbswp )  {
	nswp = 1;
	itemp = numr;
	ier = mv_swp2 ( &nswp, &itemp, &itemp );
	numr = (unsigned short)itemp;
    }
	 
/*
 *  Handle RASTER products
 */
    if  ( imrdfl == 0 )  {

/*
 *   Skip packing descriptor
 */	     
 	rwdptr += 2;

/*
 *  Determine if this is an NSSL product
 */
	ires = rmxres;
	if ( imtype < 200 ) 
	    NSSL = FALSE; 	    
	else
	    NSSL = TRUE;

/*
 *  Loop over the number of rows
 */
	for ( row = 0; row < numr; row++ )  {
	    
	    length = *((unsigned short *)rwdptr);

	    if  ( imbswp )  {
		nswp = 1;
		itemp = length;
		ier = mv_swp2 ( &nswp, &itemp, &itemp );
		length = (unsigned short)itemp;
	    }

	    rwdptr += 2;
		
/*
 *  Run Length decode this row. 
 *  Note:  1 km mosaics from UNISYS use an 8-bit run 8-bit color RLE scheme.
 */
        if ( ires == 1 && !NSSL ) length = length/2;
 
	jj = 0;
	for ( run = 0; run < length; run++ )  {

	    if  ( (int)jj > imnpix )  {
                *iret = G_NIMGFMT;
                return;
            }

	    if ( ires == 1 && !NSSL ) {
		drun = *rwdptr;
		rwdptr++;
		dcode = *rwdptr;
		rwdptr++;
	    }
	    else {
		drun  = *rwdptr >> 4;
		dcode = *rwdptr & 0xf;
		rwdptr++;
	    }

	    for ( ii = 0; ii < drun; ii++ )  {
		*imdptr++ = dcode; 
		jj++;
	    }
	}
/*
 *  Make sure the row is completed.
 */
	for ( ii = jj; (int)ii < imnpix; ii++ )
	*imdptr++ = 0; 
		
/*
 *  End of row processing
 */
	}
    }
	
/*
 *  Handle RADIAL products
 */
     else {

/*
 *  Set beam ends to zero for rasterization overflows
 *  (I don't know if we really need this.)
 */
	nbend = imnpix / 2;
	radial = (unsigned char *)malloc((nbend+2)*sizeof(unsigned char));
	radial [nbend] = 0;
	radial [nbend+1] = 0;
	  	    
/*
 *  Loop over the number of radials
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
 *  Run Length decode this radial
 */
		ib = 0;
		for ( run = 0; run < (unsigned short)(2*length); run++ )  {

		    if ( ib > nbend ) {
			*iret = G_NIMGFMT;
			free ( radial );
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
 *  Insert this radial in the rasterized image
 */
		crastr ( radial, ib, imnpix,
			 (float)rstang/10., (float)rdlang/10., &ier);
    	    }
	    free ( radial );
 	}
}
