#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_bofile ( char *filename, int *iret )
/************************************************************************
 * clo_bofile								*
 *									*
 * This function opens a bounds data file. If successful, the file 	*
 * pointer is set to fpBnd. Return codes are the same as cfl_tbop.	*
 *									*
 * clo_bofile ( filename, iret )					*
 *									*
 * Input parameters:							*
 *	*filename 	char		File name			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					= 0  - normal			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 4/01	Created					*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
    *iret = 0;

    fpBnd = cfl_tbop(filename, "bounds", iret);

}
