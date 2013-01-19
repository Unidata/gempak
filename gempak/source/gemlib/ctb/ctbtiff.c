#include "geminc.h"
#include "gemprm.h"

void ctb_tiff ( char wmoid[], char descr[], int *kbit, int *klin, 
						int *krot, int *iret )
/************************************************************************
 * ctb_tiff								*
 *									*
 * This subroutine reads the TIFF product definition table for the 	*
 * requested product. The information for the given WMO ID is returned.	*
 *									*
 * ctb_tiff ( wmoid, descr, kbit, klin,	krot, iret )			*
 *									*
 * Input parameters:							*
 *	wmoid []	char		TIFF product WMO ID		*
 *									*
 * Output parameters:							*
 *	descr []	char		TIFF product description	*
 *	*kbit		int		Number of bits per raster line	*
 *	*klin		int		Number of raster lines		*
 *	*krot		int		Rotation, in degrees		*
 *	*iret		int		Return code			*
 *					  -1 = table cannot be opened	*
 *					  -2 = entry not found		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 1/99	Copied from ctb_prod			*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 ***********************************************************************/
{
	FILE	*ftbl;
	char	tblnam[72], dirsym[72], buffer[133],
		twmo[5], tdsc[41];
	int	ibit, ilin, irot, ier;

/*---------------------------------------------------------------------*/

/*
 *	Initialize output variables.
 */
	*iret = -2;
	descr[0] = CHNULL;
	*kbit = IMISSD;
	*klin = IMISSD;
	*krot = IMISSD;

/*
 *	Open the table file.
 */
	strcpy ( tblnam, "tiffprod.tbl" );
	strcpy ( dirsym, "pgen" );
	ftbl = cfl_tbop ( tblnam, dirsym, &ier );
	if  ( ier != 0 )  {
	    *iret = -1;
	    return;
	}

/*
 *	Read in the next record, check for a comment,
 *	and process valid table entries.
 */
	while ( fgets ( buffer, 132, ftbl ) != NULL )  {
	    if  ( buffer[0] != '!' )  {
		if  ( sscanf ( buffer, "%6s %40c %d %d %d",
			       &twmo[0], &tdsc[0],
			       &ibit, &ilin, &irot ) >= 5 )  {
		    if  ( strcmp ( wmoid, twmo ) == 0 )  {
			cst_ncpy ( descr, tdsc, 40, &ier );
			*kbit = ibit;
			*klin = ilin;
			*krot = irot;
			*iret = 0;
		    }
		}
	    }
	}

	cfl_clos ( ftbl, &ier );
}
