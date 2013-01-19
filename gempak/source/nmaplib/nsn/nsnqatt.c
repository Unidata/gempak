#include "nsncmn.h"

void nsn_qatt ( int index, char alias[], int *isbcat, char cycle[], 
		char parms[], char color[], char level[], char vcord[], 
		char filter[], char txtatt[], int *iret )
/************************************************************************
 * nsn_qatt								*
 *									*
 * This routine queries the attributes for the SND data. The index	*
 * tells which set of attributes to query.				*
 *									*
 * nsn_qatt ( index, alias, isbcat, cycle, parms, color, level, vcord,	*
 *	      filter, txtatt, iret )					*
 *									*
 * Input parameters:							*
 *	index		int		Input index to arrays		*
 *									*
 * Output parameters:							*
 *	alias[]		char		Alias for SND data		*
 *	*isbcat		int		Data subcategory number		*
 *	cycle[]		char		Cycle time for the data		*
 *	parms[]		char		Parameter list			*
 *	color[]		char		Color list			*
 *	level[]		char		Data level			*
 *	vcord[]		char		Vertical coordinate		*
 *	filter[]	char		Filter value			*
 *	txtatt[]	char		Text attributes			*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXSND -> MAXTMPLT			*
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
	strcpy ( alias,  snddt[index].alias );
	*isbcat = snddt[index].isbcat;
	strcpy ( cycle,  snddt[index].cycle );
	strcpy ( parms,  snddt[index].parms );
	strcpy ( color,  snddt[index].color );
	strcpy ( level,  snddt[index].level );
	strcpy ( vcord,  snddt[index].vcord );
	strcpy ( filter, snddt[index].filter );
	strcpy ( txtatt, snddt[index].txtatt );

}
