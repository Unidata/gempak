#include "nsfcmn.h"

void nsf_qatt ( int index, char alias[], int *isbcat, 
		char cycle[], char parms[], char color[], 
		char filter[], char txtatt[], int * iret )
/************************************************************************
 * nsf_qatt								*
 *									*
 * This routine queries the attributes for the SFC data. The index	*
 * tells which set of attributes to query.				*
 *									*
 * nsf_qatt ( index, alias, isbcat, cycle, parms, color, filter,	*
 *	      txtatt, iret )						*
 *									*
 * Input parameters:							*
 *	index		int		Input index to arrays		*
 *									*
 * Output parameters:							*
 *	alias[]		char		Alias for SFC data		*
 *	*isbcat		int		Data subcategory number		*
 *	cycle[]		char		Cycle time for the data		*
 *	parms[]		char		Parameter list			*
 *	color[]		char		Color list			*
 *	filter[]	char		Filter value			*
 *	txtatt[]	char		Text attributes			*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXSFC -> MAXTMPLT			*
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
	strcpy ( alias,  sfcdt[index].alias );
	*isbcat = sfcdt[index].isbcat;
	strcpy ( cycle,  sfcdt[index].cycle );
	strcpy ( parms,  sfcdt[index].parms );
	strcpy ( color,  sfcdt[index].color );
	strcpy ( filter, sfcdt[index].filter );
	strcpy ( txtatt, sfcdt[index].txtatt );

}
