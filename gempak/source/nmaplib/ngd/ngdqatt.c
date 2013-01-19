#include "ngdcmn.h"

void ngd_qatt ( int index, char alias[], int *isbcat, 
			char cycle[], char rstfil[], int *iret )
/************************************************************************
 * ngd_qatt								*
 *									*
 * This routine queries the attributes for the GRID data. The index	*
 * tells which set of attributes to query.				*
 *									*
 * ngd_qatt ( index, alias, isbcat, cycle, rstfil, iret )		*
 *									*
 * Input parameters:							*
 *	index		int		Input index to arrays		*
 *									*
 * Output parameters:							*
 *	alias[]		char		Alias for GRID data		*
 *	*isbcat		int		Data subcategory number		*
 *	cycle[]		char		Cycle time for the data		*
 *	rstfil[]	char		Restore file (full pathname)	*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXGRID -> MAXTMPLT			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = 0;

/*
 *	Check for a valid index value.
 */
	if  ( index < 0 || index >= MAXTMPLT )  {
	    *iret = -1;
	    return;
	}

/*
 *	Set the output values to the stored values.
 */
	strcpy ( alias,  grddt[index].alias );
	*isbcat = grddt[index].isbcat;
	strcpy ( cycle,  grddt[index].cycle );
	strcpy ( rstfil, grddt[index].rstfil );

}
