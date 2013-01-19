#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

float   cdsPipSiz;
float   cdsFrntStrngth;

float   cdsSymWdth;
float   cdsSymSiz;

float   cdsWindWdth;
float   cdsWindSiz;
float   cdsWindHdsiz;

float   cdsLineWdth;

float   cdsSplWdth;
float   cdsSplSiz;

float   cdsTxtSiz;
float   cdsTxtWdth;

float   cdsSptSiz;
float   cdsSptWdth;

void cds_ress ( int *iret )
/************************************************************************
 * cds_ress								*
 *									*
 * This function resets the scaling values for the cds library.		*
 *									*
 * cds_ress ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * C.Lin/EAI		03/98						*
 * I. Durham/GSC	05/98	Changed underscore decl. to an include  *
 * F. J. Yen/NCEP	05/98	Removed pip stroke			*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	*iret        = 0;
    
    	cdsPipSiz      = 1.0F;
	cdsFrntStrngth = 1.0F;

	cdsSymWdth   = 1.0F;
	cdsSymSiz    = 1.0F;

	cdsWindWdth  = 1.0F;
	cdsWindSiz   = 1.0F;
	cdsWindHdsiz = 1.0F;

	cdsLineWdth  = 1.0F;

	cdsSplWdth   = 1.0F;
	cdsSplSiz    = 1.0F;

	cdsTxtSiz    = 1.0F;
	cdsTxtWdth   = 1.0F;

	cdsSptSiz    = 1.0F;
	cdsSptWdth   = 1.0F;
}
