#include "nmscmn.h"

void nms_satt ( int iindex, char alias[], int isbcat, 
		char filnam[], int ntype, NMS_types types[], 
		int nflag, NMS_flags flags[], int *jindex, int *iret )
/************************************************************************
 * nms_satt								*
 *									*
 * This routine sets the attributes for the MISC data. The input index	*
 * tells which set of attributes to set. If the value is < 0, the next	*
 * available index is used.						*
 *									*
 * nms_satt ( iindex, alias, isbcat, filnam, ntype, types, nflag, flags,*
 * 	      jindex, iret )						*
 *									*
 * Input parameters:							*
 *	iindex		int		Input index to arrays		*
 *					   < 0 = Choose next available	*
 *	alias[]		char		Alias for MISC data		*
 *	isbcat		int		Data subcategory number		*
 *	filnam[]	char		Data file name, if necessary	*
 *	ntype		int		Number of types			*
 *	types []	NMS_types	Editable data settings		*
 *	nflag		int		Number of flags			*
 *	flags []	NMS_flags	Editable flag settings		*
 *									*
 * Output parameters:							*
 *	*jindex		int		Output index to arrays		*
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
 * T. Piper/SAIC        06/03   Allowed for removing an index           *
 * F. J. Yen/NCEP	 6/04	Added arrw.ityp				*
 ***********************************************************************/
{

	int		ii, jj, knt, found, ier;
/*---------------------------------------------------------------------*/

	*iret = 0;

/*
 *	Start with the index provided.
 */
	*jindex = iindex;

/*
 *	If the index is less than 0, pick the first available index.
 */
	if  ( *jindex < 0 )  {

/*
 *	    Set the first available value for the index.
 */
	    knt = 0;
	    while ( ( knt < MAXTMPLT ) && 
		    ( mscdt[knt].alias[0] != CHNULL ) ) {
		knt++;
	    }

	    *jindex = knt;
	}

/*
 *	If the index is bigger than the maximum value allowed,
 *	return with an error.
 */
	if  ( *jindex >= MAXTMPLT )  {
	    *iret = -1;
	    return;
	}

/*
 *	Set the structure values to the input values.
 */
	cst_lcuc ( alias, mscdt[*jindex].alias, &ier );
	mscdt[*jindex].isbcat = isbcat;
	strcpy ( mscdt[*jindex].filnam, filnam );

	for ( ii = 0; ii < ntype; ii++ )  {
	  strcpy ( mscdt[*jindex].msctyp[ii].name, types[ii].name );
	  mscdt[*jindex].msctyp[ii].ionoff = types[ii].ionoff;
	  mscdt[*jindex].msctyp[ii].icolr  = types[ii].icolr;
	  mscdt[*jindex].msctyp[ii].icolr2  = types[ii].icolr2;
	  mscdt[*jindex].msctyp[ii].value  = types[ii].value;

	  mscdt[*jindex].msctyp[ii].line.size = types[ii].line.size;
	  mscdt[*jindex].msctyp[ii].line.iwid = types[ii].line.iwid;

	  for ( jj = 0; jj < 2; jj++ )  {
	   mscdt[*jindex].msctyp[ii].symb[jj].code = types[ii].symb[jj].code;
	   mscdt[*jindex].msctyp[ii].symb[jj].size = types[ii].symb[jj].size;
	   mscdt[*jindex].msctyp[ii].symb[jj].iwid = types[ii].symb[jj].iwid;
	  }

	  mscdt[*jindex].msctyp[ii].arrw.size = types[ii].arrw.size;
	  mscdt[*jindex].msctyp[ii].arrw.hdsz = types[ii].arrw.hdsz;
	  mscdt[*jindex].msctyp[ii].arrw.iwid = types[ii].arrw.iwid;
	  mscdt[*jindex].msctyp[ii].arrw.ityp = types[ii].arrw.ityp;
	}
	mscdt[*jindex].numtyp = ntype;

	for ( ii = 0; ii < nflag; ii++ )  {
	  strcpy ( mscdt[*jindex].mscflg[ii].name, flags[ii].name );
	  mscdt[*jindex].mscflg[ii].iflg = flags[ii].iflg;
	}
	mscdt[*jindex].numflg = nflag;

/*
 *	Check for the index already in the list of indices.
 */
	found = G_FALSE;
	for ( knt = 0; knt < MAXTMPLT; knt++ ) {
	    if  ( indmsc[knt] == *jindex )  {
		found = G_TRUE;
		break;
	    }
	}

/*
 *	If this is a new index, set the index value in the
 *	global array and increase the count of indices set.
 *      If it is null remove index.
 */
	if  ( ! found ) {
	    indmsc[*jindex] = *jindex;
	}
        else {
            if (mscdt[*jindex].alias[0] == CHNULL  ) {
                indmsc[*jindex] = -1;
            }
        }
}
