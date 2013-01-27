#include "geminc.h"
#include "gemprm.h"

#include "dcgrib.h"

/************************************************************************
C* NAGCUT                                                               *
C*                                                                      *
C* This subroutine removes the padded values on the right side of a     *
C* staggered eta grid labeled 203 in GDS octet 6.                       *
C*                                                                      *
C* The input and output grids may be the same!                          *
C*                                                                      *
C* NAGCUT  ( SG, IXF, IYF, PARM, SGO, IRET )                            *
C*                                                                      *
C* Input parameters:                                                    *
C*      SG (*)          REAL            Staggered grid                  *
C*      IXF             INTEGER         Filled grid x dimension         *
C*      IYF             INTEGER         Filled grid y dimension         *
C*      PARM            CHAR*           Parameter name                  *
C*                                                                      *
C* Output parameters:                                                   *
C*      SGO (*)         REAL            Staggered grid w/o pad values   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* K. Brill/NMC          9/98                                           *
C************************************************************************/

void stagger_e ( float *ingrid, float *fgrid, int npts, int kx, int ky, 
		 char *parm, int *iret )
{
int vgrd, imod1, imod2, mod1, mod2;
int ixs, ngmx, ii=0, ig;

*iret = 0;

if ( ( strncmp ( parm, "UREL", 4 ) == 0 ) ||
     ( strncmp ( parm, "VREL", 4 ) == 0 ) ) /* assume other UREL_stuff too! */
   vgrd = 1;
else
   vgrd = 0;

ixs = ( kx / 2 ) + 1;
ngmx = ixs * ky;

if ( vgrd )
   {
   imod1 = ixs;
   imod2 = ixs * 2;
   for ( ig = 1; ig <= ngmx; ig++)
      {
      mod1 = ig % imod1;
      mod2 = ig % imod2;
      if  ( ( mod1 != 0 || mod2 == 0 ) )
         {
         ingrid[ii] = ingrid[ig - 1];
         ii++;
         }
      }
   }
else
   {
   imod1 = ixs * 2;
   for ( ig = 1; ig <= ngmx; ig++)
      {
      if ( ( ig % imod1 ) != 0 )
         {
         ingrid[ii] = ingrid[ig - 1];
         ii++;
         }
      }
   }

if ( vgrd )
   staggerh ( ingrid, fgrid, kx, ky );
else
   staggerv ( ingrid, fgrid, kx, ky );

}

void stagger ( float *ingrid, float *fgrid, int npts, int kx, int ky, 
		 char *parm, int *iret )
{
int vgrd;

*iret = 0;

if ( ( strncmp ( parm, "UREL", 4 ) == 0 ) ||
     ( strncmp ( parm, "VREL", 4 ) == 0 ) ) /* assume other UREL_stuff too! */
   vgrd = 1;
else
   vgrd = 0;
if ( vgrd )
   staggerh ( ingrid, fgrid, kx, ky );
else
   staggerv ( ingrid, fgrid, kx, ky );
}
