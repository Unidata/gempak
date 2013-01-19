#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"

void cgr_ordrccw ( int npts, float *xpt, float *ypt, int *iret )
/************************************************************************
 * cgr_ordrccw								*
 *                                                                      *
 * Re-order (npts) points in a counter clockwise direction.		*
 *                                                                      *
 * cgr_ordrccw ( npts, xpt, ypt, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 * 	npts		int		number of points		* 
 *									*
 * Input/Output parameters:                                             *
 *	*xpt		float		array of x coordinates		*
 *	*ypt		float		array of y coordinates		*
 *									*
 * Output parameters:							*
 *	*iret		int		return code			*
 *					  0 = normal			*
 *					 -1 = error       		*
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		12/04	initial coding				*
 ***********************************************************************/
{
   float	*tmpX, *tmpY;
   int		*index, ii, ier;
/*---------------------------------------------------------------------*/

   if ( npts <= 0 ) {
      *iret = -1;
      return;
   }

   *iret = 0;

   /*
    *  write the x and y values into temporary arrays.
    */
   tmpX   = ( float * )malloc( npts * sizeof(float) );
   tmpY   = ( float * )malloc( npts * sizeof(float) ); 

   for ( ii=0; ii < npts; ii++ ) {
      tmpX[ii] = xpt[ii];
      tmpY[ii] = ypt[ii];
   }

   index  = ( int * )malloc( npts * sizeof(int) );

   cgr_reorder( &npts, tmpX, tmpY, index, &ier );

   /*
    *  reorder the xpt and ypt arrays using the index values.
    */
   for ( ii=0; ii < npts; ii++ ) {
      xpt[ii] = tmpX[index[ii]];
      ypt[ii] = tmpY[index[ii]];
   } 


   if ( index ) free( index );
   if ( tmpX  ) free( tmpX );
   if ( tmpY  ) free( tmpY );
}


