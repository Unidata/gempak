#include "nmscmn.h"

void nms_qatt ( int index, char alias[], int *isbcat, 
		char filnam[], int *ntype, NMS_types types[], 
		int *nflag, NMS_flags flags[], int *iret )
/************************************************************************
 * nms_qatt								*
 *									*
 * This routine queries the attributes for the MISC data. The index	*
 * tells which set of attributes to query.				*
 *									*
 * nms_qatt ( index, alias, isbcat, filnam, ntype, types, nflag, flags,	*
 *	      iret )							*
 *									*
 * Input parameters:							*
 *	index		int		Input index to arrays		*
 *									*
 * Output parameters:							*
 *	alias[]		char		Alias for MISC data		*
 *	*isbcat		int		Data subcategory number		*
 *	filnam[]	char		Data file name			*
 *	*ntype		int		Number of types			*
 *	types []	NMS_types	Editable data settings		*
 *	*nflag		int		Number of flags			*
 *	flags []	NMS_flags	Editable flag settings		*
 *	*iret		int		Return code			*
 *					   -1 = invaid index value	*
 *									*
 **									*
 * S. Jacobs/NCEP	 9/99	Created					*
 * S. Jacobs/NCEP	11/99	Changed arrays of info to structures	*
 * S. Jacobs/NCEP	12/99	Updated data structures			*
 * S. Jacobs/NCEP	 3/00	Updated data structures			*
 * S. Law/GSC		06/00	MAXMISC -> MAXTMPLT			*
 * M. Li/SAIC		04/03	Added icolr2				*
 * F. J. Yen/NCEP	 6/04	Added ityp for arrw			*
 ***********************************************************************/
{

	int	ii, jj;

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
	strcpy ( alias,  mscdt[index].alias );
	*isbcat = mscdt[index].isbcat;
	strcpy ( filnam, mscdt[index].filnam );

	*ntype = mscdt[index].numtyp;
	for ( ii = 0; ii < *ntype; ii++ )  {
	  strcpy ( types[ii].name, mscdt[index].msctyp[ii].name );
	  types[ii].ionoff = mscdt[index].msctyp[ii].ionoff;
	  types[ii].icolr  = mscdt[index].msctyp[ii].icolr;
	  types[ii].icolr2  = mscdt[index].msctyp[ii].icolr2;
	  types[ii].value  = mscdt[index].msctyp[ii].value;

	  types[ii].line.size = mscdt[index].msctyp[ii].line.size;
	  types[ii].line.iwid = mscdt[index].msctyp[ii].line.iwid;

	  for ( jj = 0; jj < 2; jj++ )  {
	   types[ii].symb[jj].code = mscdt[index].msctyp[ii].symb[jj].code;
	   types[ii].symb[jj].size = mscdt[index].msctyp[ii].symb[jj].size;
	   types[ii].symb[jj].iwid = mscdt[index].msctyp[ii].symb[jj].iwid;
	  }

	  types[ii].arrw.size = mscdt[index].msctyp[ii].arrw.size;
	  types[ii].arrw.hdsz = mscdt[index].msctyp[ii].arrw.hdsz;
	  types[ii].arrw.iwid = mscdt[index].msctyp[ii].arrw.iwid;
	  types[ii].arrw.ityp = mscdt[index].msctyp[ii].arrw.ityp;
	}

	*nflag = mscdt[index].numflg;
	for ( ii = 0; ii < *nflag; ii++ )  {
	    strcpy ( flags[ii].name, mscdt[index].mscflg[ii].name );
	    flags[ii].iflg = mscdt[index].mscflg[ii].iflg;
	}

}
