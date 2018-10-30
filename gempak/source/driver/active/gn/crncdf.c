#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"
#include "netcdf.h"
#include "proto_xw.h"

void crncdf ( char *imgnam, int *iret )
/************************************************************************
 * crncdf								*
 *									*
 * This subroutine reads the image data from an AWIPS NetCDF file.	*
 * The full image is placed in memory pointed to by imgData.		*
 *									*
 * crncdf ( imgnam, iret )						*
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
 * S. Jacobs/NCEP	 6/99	Created					*
 * R. Curtis/EAI	 9/00   Implemented into GEMPAK                 *
 * S. Chiswell/Unidata	 4/05	Added QNM reading			*
 * S. Guan/NCEP          8/15   Modified the code for Sectorized_CMI    *
 *                              data                                    *
 * S. Guan/NCEP          9/17   Take care bit_depth  bits netcdf4 data  *   
 ***********************************************************************/
{

	int		ncid, ncret;
	int             data_id;
	short           *pdatac;
	double          bit_depth;
	int             i, total, move;
	int		ival, ier=0, ivarid;
	int		itype=IMISSD;
	float		fscale, fmissing, fval;
	float		minpx, maxpx, maxdv, mindv;
	size_t		start[4], count[4];
	short		*svals;
	FILE		*fp;
	char		buf[256];

/*---------------------------------------------------------------------*/
	
	*iret = G_NORMAL ;

/*
 *	Open the NetCDF file.
 */
	nc_open ( imgnam, NC_NOWRITE, &ncid );

/*
 *	Read the image data.
 */

	/* qpe read */
	if ( imradf == 1 )
	   {
	   /* netcdf variable ID stored im imdoff */
	   ivarid = imdoff;

	   if ( (fp = (FILE *)cfl_tbop("radnetcdf.tbl","unidata",&ier) ) != NULL )
             {
             ier = 0;
             while(ier == 0)
                {
                cfl_trln(fp,255,buf,&ier);
                if ( ier != 0 ) continue;

                sscanf ( buf+20, "%d %f,%f,%f,%f", 
			&itype, &minpx, &maxpx, &mindv, &maxdv);
		if ( itype == imtype ) break;
                }
             cfl_clos ( fp, &ier);
             if ( itype != imtype )
                {
                printf("unknown netcdf qpe parameter %d\n",imtype);
		minpx = mindv = 0.;
		maxpx = maxdv = 255.;
                }
             }
           else
              {
              printf("unable to open radnetcdf.tbl\n");
	      minpx = mindv = 0.;
	      maxpx = maxdv = 255.;
              }

	   if ( ( svals = (short *)malloc(imnlin*imnpix*sizeof(short)) ) != NULL )
	      {
	      if ( (ier = nc_get_att_float ( ncid, ivarid, "Scale", &fscale ) ) != 0 )
		fscale = 1.0;
	      if ( (ier = nc_get_att_float ( ncid, ivarid, "MissingData", &fmissing ) ) != 0 )
		fmissing = RMISSD;

	      start[1] = 0;
	      count[0] = 1;
	      count[1] = imnpix;

	      /* Read lines, row by row and flip to lower left order */
	      /*for ( i = 0; i < imnlin && ier == 0; i++ )
		 {
		 start[0] = i;
	         ier = nc_get_vara_short ( ncid, ivarid, start, count, &svals[(imnlin - 1 - i)*imnpix] );
		 }*/

	      start[0] = 0;
	      count[0] = imnlin;
	      ier = nc_get_vara_short ( ncid, ivarid, start, count, svals );
	      if ( ier != 0 )
	         {
		 printf("failed to get svals array ivarid %d\n", ivarid);
		 *iret = G_NIMGFMT;
		 }
	      else
		 {
	         for ( i = 0; i < imnlin*imnpix; i++)
		    {
		    fval = (float)svals[i] / fscale;
		    
		    if ( fval <= fmissing )
		       imgData[i] = immnpx;
		    else if ( fval <= mindv )
                          imgData[i] = immnpx;
	            else if ( fval >= maxdv )
		          imgData[i] = immxpx;
		    else
		       {
		       ival = (int)((immxpx - immnpx) * ( fval - mindv) / ( maxdv - mindv ));
                       imgData[i] = (unsigned char)ival;
		       }
		    }
		 }	
	      free(svals);
	      }
 	   }
	else
	   {
	   start[0] = 0;
	   start[1] = 0;
	   count[0] = imnlin;
	   count[1] = imnpix;
	   total = imnlin * imnpix; 
	   ncret = nc_inq_varid (ncid, "Sectorized_CMI", &data_id);
	   if ( ncret != 0 ) data_id = 0;
	   ncret = nc_get_att_double ( ncid, NC_GLOBAL, "bit_depth", &bit_depth );
	   if ( ncret != 0 ) bit_depth = 0;
	   if ( bit_depth < 8.1) {
		nc_get_vara_uchar ( ncid, data_id, start, count, imgData );
	   } else {
  /*          imgData is 8 bits, netcdf4 data is bit_depth  bits */
		pdatac =  malloc(imnlin * imnpix * sizeof (short));
		nc_get_vara_short ( ncid, data_id, start, count, pdatac );
		move = (int) bit_depth - 8;
		for (i = 0; i < total; i++) { 
		   imgData[i] = pdatac[i] >> move;
		}    
	   }
    
	}
/*
 *	Close the NetCDF file.
 */
	nc_close ( ncid );
	
}
