#include "geminc.h"
#include "gemprm.h"
#define CDS_GLOBAL
#include "cds.h"

int	cdsColor;
int	cdsFill;
float	cdsSmthDens[3];

void cds_init ( int *iret )
/************************************************************************
 * cds_init								*
 *									*
 * This function initializes values for the cds library.		*
 *									*
 * cds_init ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/97	Created					*
 * D.W.Plummer/NCEP	10/97	Added intialization for cdsFill		*
 * C.Lin/EAI		03/98	Added attribute scaling factors		*
 * S.Law/GSC		04/98	Added smoothing factors			*
 * S. Jacobs/NCEP	 4/98	Modified smoothing factors		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * F. J. Yen/NCEP	 5/98	Retyped cdsColor and cdsFill to char	*
 * D.W.Plummer/NCEP	 6/98	Retyped cdsColor and cdsFill to int	*
 * F. J. Yen/NCEP	10/99	Handled user attribute table		*
 * F. J. Yen/NCEP	10/99	Replaced cds_rtbl with cds_atdeflt	*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 ***********************************************************************/
{
    int ier;
/*---------------------------------------------------------------------*/

    *iret       = 0;

    cdsColor    = 0;
    cdsFill     = 1;

    cdsSmthDens[0] = 0.0F;
    cdsSmthDens[1] = 1.0F;
    cdsSmthDens[2] = 5.0F;

    cds_ress(&ier);

    /*
     *  Invoke cds_atdeflt to set the default values in the
     *  global attribute structure cdsUattr.
     */

    cds_atdeflt ( &ier );

}
