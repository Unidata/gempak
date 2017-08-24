#include "df.h"

#define DEG            57.3

void df_nwavg ( int *iret )
/************************************************************************
 * df_nwavg								*
 *									*
 * This subroutine computes NWAVG (S,ROI), the neighborhood, weighted   *
*  average value of a scalar field (S) within some radius of influence	*
 * (ROI; meters). Masking could be used [e.g., SGT(S1,S2)] to subset    *
 * and filter the grid beforehand to allow for faster processing.      	*
 *									*
 * df_nwavg ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * C. Melick/SPC	09/16						*
 ************************************************************************/
{
    int num1, num2, num3, num, kxd, kyd, ksub1, ksub2, zero, indx, ier;
    int ixmscl, iymscl, jgymin, jgymax, jgxmin, jgxmax, idglat, idglon;
    int row, col, ibeg, iend, jbeg, jend, ibox, jbox, boxindx, nval;
    float  gddx, gddy, gdspdx, gdspdy, radius, numberpts;
    float *gnum1, *gnumn, *gkxms, *gkyms, *gnumroi, *glat, *glon, *dist;   
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Compute map scale factors.
     */
    dg_mscl ( iret );
    if ( *iret != 0 ) return;

    /*
     * Query DGCMN.CMN idglat/idglon.
    */
     
    nval = 1;
    dg_iget ( "IDGLAT", &nval, &idglat, iret );
    if ( *iret != 0 ) return;
    dg_iget ( "IDGLON", &nval, &idglon, iret );
    if ( *iret != 0 ) return;

    /*
     * Get the grids from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &num2, iret );
    if ( *iret != 0 ) return;

    /*
     * Get a new grid number.
     */
    dg_nxts ( &num3, iret );
    if ( *iret != 0 ) return; 
    dg_nxts ( &num, iret );
    if ( *iret != 0 ) return;

    dg_qmsl ( &ixmscl, &iymscl, &gddx, &gddy, &ier );
    dg_qbnd ( &jgxmin, &jgxmax, &jgymin, &jgymax, &ier );   
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &num,  &gnumn,  &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &ixmscl, &gkxms, &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &iymscl, &gkyms, &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &num2, &gnumroi, &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &idglat, &glat, &kxd, &kyd, &ksub1, &ksub2, &ier );
    dg_getg ( &idglon, &glon, &kxd, &kyd, &ksub1, &ksub2, &ier ); 
    dg_getg ( &num3, &dist, &kxd, &kyd, &ksub1, &ksub2, &ier ); 

    radius = gnumroi[0];

    /*  QC check on lower and upper bounds of radius of influence.  */

    if ( radius < 0 ) {
         radius = 0.0;
         printf ("\n WARNING : RADIUS value less than zero.  "
                 "Resetting to zero.\n");
    }

    if ( radius > 0.5*gddx*(float)(kxd)) {
         radius = 0.5*gddx*(float)(kxd);
         printf ("\n WARNING : RADIUS value too high.  "
                 "Resetting to half the distance in X (%f meters).\n",radius);
    } 

    /*
     * Loop over all grid points to initialize output grid.
     */
    for ( row = jgymin; row <= jgymax; row++ ) {
	for ( col = jgxmin; col <= jgxmax; col++ ) {
            indx=(row-1)*kxd+(col-1);
            if ( ERMISS ( gnum1[indx] ) ) {
		gnumn[indx] = RMISSD;
            } else {
                gnumn[indx] = 0.0;
            }
        }
    }

    /*
     * Loop over all grid points to determine neighborhood average for each grid point.
     */

    for ( row = jgymin; row <= jgymax; row++ ) {
       for ( col = jgxmin; col <= jgxmax; col++ ) {
         indx=(row-1)*kxd+(col-1);
         numberpts=0;
         if ( ! ERMISS ( gnum1[indx] ) ) {
                gdspdx= gddx / gkxms[indx];
                gdspdy= gddy / gkyms[indx];
    
       /*  Constructing box for each grid point */
                ibeg = col- G_NINT(radius / gdspdx);
                iend = col+ G_NINT(radius / gdspdx);
                jbeg = row- G_NINT(radius / gdspdy);
                jend = row+ G_NINT(radius / gdspdy);
                if (ibeg < jgxmin) {
                   ibeg = jgxmin;
                }
                if (iend > jgxmax) {
                   iend = jgxmax;
                }
                if (jbeg < jgymin) {
                   jbeg = jgymin;
                }
                if (jend > jgymax) {
                   jend = jgymax;
                }
                for ( ibox = ibeg; ibox <= iend; ibox++ ) {
                    for ( jbox = jbeg; jbox <= jend; jbox++ ) {
                        boxindx=(jbox-1)*kxd+(ibox-1);
                        if ((glat[indx] == glat[boxindx]) && (glon[indx] == glon[boxindx])) {
                            dist[boxindx]=0.0;
                        } else {
        /* Great Circle Distance calculation */
                            dist[boxindx] = acos(sin(glat[boxindx])*sin(glat[indx]) + cos(glat[boxindx])*cos(glat[indx])*cos((glon[boxindx])-(glon[indx])));
                            dist[boxindx] = RADIUS * dist[boxindx];
                        }
/* Calculate weighted sum if neighboring point is defined and within radius of influence. */
                        if ( (dist[boxindx] <= radius) && (! ERMISS ( gnum1[boxindx] ) ) ) { 
                                 gnumn[indx] = gnum1[boxindx] * ( (radius + gddx - dist[boxindx]) / ( radius + gddx ) ) + gnumn[indx];
                                numberpts= numberpts + ( (radius + gddx - dist[boxindx]) / ( radius + gddx ) );
                        }   
                    }
                }
/* Calculate final average */
               gnumn[indx] = gnumn[indx] / numberpts;
          }
       }
    } 

    /*
     * Make a name of the form 'NWAVG'//S and update header;
     * update stack.
     */
    dg_updh ( "NWAVG", &num, &num1, &num2, iret );
    dg_puts ( &num, iret );
    dg_esub ( &num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
} 
