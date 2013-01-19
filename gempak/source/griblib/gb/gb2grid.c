#include "gb2def.h"

void gb2_grid ( Gribmsg *g2, int iuscal, 
		float rmsval, int *iprec, 
		float *fgrid, int *iret )
/************************************************************************
 * gb2_grid                                                             *
 *                                                                      *
 * This function decodes the current GRIB message and returns an        *
 * unpacked grid of floating point numbers.                             *
 *                                                                      *
 * gb2_grid ( g2, iuscal, rmsval,                                       *
 *           iprec, fgrid, iret )					*
 *                                                                      *
 * Input parameters:							*
 *      *g2         struct gribmsg      GRIB message structure          *
 *      iuscal		int		GEMPAK units scale factor       *
 *	rmsval		float		Missing data value		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iprec		int		precision to use for packing	*
 *      *fgrid		float		Unpacked grid data              *
 *      *iret		int		Return code                     *
 *					    -18 = Invalid bitmap	*
 *					    -33 = Error unpacking field *
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI           7/93                                           *
 * S. Jacobs/EAI	10/93	Changed memmove to memcpy		*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * K. Brill/NMC          6/94   Fixes for BITMAP grids          	*
 * L. Sager/NMC          8/95   Fix memory allocation error     	*
 * K. Brill/NMC		10/95	Do not increase bds.num_bits =1 	*
 * K. Brill/EMC		12/95	If nbits = 1 increment by 1		*
 * S. Jacobs/NCEP	 1/96	Changed DA_READ to CFL_READ		*
 * D.W.Plummer/NCEP	 2/96	Cleanup GBDIAGs and comments		*
 * D.W.Plummer/NCEP	 3/96	Changes cfl_ call sequence		*
 * D.W.Plummer/NCEP	 8/96	Temporary ECMWF processing fix  	*
 * D.W.Plummer/NCEP	12/96	Added test for bdslength<=0 (iret=-22)	*
 * S. Jacobs/NCEP	 8/98	Changed igrid from long int to int	*
 * T. Piper/GSC		11/98	Updated prolog				*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 * S. Gilbert            1/04   Modified from gb_gubd.c for use with    *
 *                              GRIB2.                                  *
 * S. Gilbert/NCEP          03/06    Chngs to remove compiler warnings  *
 ***********************************************************************/
{
    register int	i;

    int         iret2, nmiss;

    float	sfact, rmiss[2];
    double      more;

    int         num,idec,ibin;
    g2int       unpack,expand;
    gribfield   *tmpfld=0, *ufld;

/*---------------------------------------------------------------------*/

    *iret = 0;
    unpack=1;
    expand=1;


    /*
     *	   Check if a predefined bit map is required.
     *      This not yet implemented.
     */
    if ( g2->gfld->ibmap >= 1 && g2->gfld->ibmap <= 253 ) {
	*iret = -18;
	return;
    }

    /*
     *	Unpack the bitmap (if included) and gridpoint
     *  data values for the current GRIB2 field.
     */

    tmpfld=g2->gfld;
    num=g2->gfld->ifldnum;

    iret2 = (int)g2_getfld( g2->cgrib2, num, unpack, expand, &g2->gfld);
    if ( iret2 != 0 ) {
       *iret=-33;
       g2->gfld=tmpfld;
       return;
    } 
    else {
       g2_free(tmpfld);
    }
    ufld = g2->gfld;
   
    /*
     *	Scale data values with gempak scale value.
     */

    if ( iuscal != 0 ) {
       sfact=(float)pow (10.0,(double)iuscal);
       for (i=0; i<ufld->ngrdpts; i++) {
	    ufld->fld[i] = ufld->fld[i] * sfact;
       }
    }

    /*
     *	If a bitmap is included with the field, set all
     *  unmapped grid points to missing val, rmsval.
     */

    if ( ufld->ibmap == 0  || ufld->ibmap == 254 ) {
       for (i=0; i<ufld->ngrdpts; i++) {
	   if ( ufld->bmap[i] == 0 ) ufld->fld[i] = rmsval;
       }
    }

    /*
     *	If missing value management is used for DRT's 5.2 and 5.3,
     *  set missing grid points to missing val, rmsval.
     */

    if ( (ufld->idrtnum == 2  || ufld->idrtnum == 3) &&
          ufld->idrtmpl[6] != 0  ) {
       
       gb2_gmis( ufld, rmiss, &nmiss);

       if ( ufld->idrtmpl[6] == 1 ) {
          for (i=0; i<ufld->ngrdpts; i++) {
	      if ( G_DIFF (ufld->fld[i], rmiss[0] ) ) ufld->fld[i] = rmsval;
          }
       }

       if ( ufld->idrtmpl[6] == 2 ) {
          for (i=0; i<ufld->ngrdpts; i++) {
	      if ( G_DIFF ( ufld->fld[i], rmiss[0] ) ||
                    G_DIFF ( ufld->fld[i], rmiss[1] ) ) ufld->fld[i] = rmsval;
          }
       }

    }

    /*
     *  Make sure data points are oriented from lower left to upper 
     *  right by rows.
     */
    gb2_ornt( g2->kx, g2->ky, g2->g2scan_mode, ufld->fld, fgrid, iret );

    /*
     *	Bump nbits to 2 if it is 1.
     if ( *nbits == 1 ) *nbits = *nbits + 1;
     */
     ibin = (int)ufld->idrtmpl[1];      /* binary scale factor */
     idec = (int)ufld->idrtmpl[2];      /* decimal scale factor */
     *iprec = idec - iuscal;
     if ( ibin != 0 ) {
         more = ceil(-0.3*(double)ibin);
         *iprec = *iprec + (int)more;
     }
     /*printf("SAGT:%d %d %d %d %f\n",ibin,idec,iuscal,*iprec,rmsval);*/


}

