#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

void ctb_rdtyp ( char *tblnam, char *dirsym, int *maxdtyp, 
		 int *ndtyp, struct datatype_list *dtyparr, int *iret )
/************************************************************************
 * ctb_rdtyp								*
 *									*
 * This routine will read a station table into an array of structures.	*
 *									*
 * ctb_rdtyp ( tblnam, dirsym, maxdtyp, ndtyp, dtyparr, iret )		*
 *									*
 * Input parameters:							*
 *	*tblnam		char		Data type table name		*
 *	*dirsym		char		Directory path/symbol		*
 *	*maxdtyp	int		Maximum number of entries	*
 *									*
 * Output parameters:							*
 *	*ndtyp		int		Number of entries		*
 *	*dtyparr	struct datatype_list Data type list structure	*
 *	*iret		int		Return code			*
 *					   As for cfl_ropn		*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * G. Krueger/EAI	 3/96		CFL_SOPN -> CFL_ROPN		*
 * T. Piper/SAIC	01/04	replaced cfl_ropn with cfl_tbop		*
 ***********************************************************************/
{
	FILE    *ftbl;
	char    record[133];
	int	i, kdtyp;

/*---------------------------------------------------------------------*/
	*iret = 0;

/*
 *	Initialize the structure values.
 */
	for ( i = 0; i < *maxdtyp; i++ ) {
	    strcpy ( dtyparr[i].datatyp, " " );
	    strcpy ( dtyparr[i].loctbl,  " " );
	    strcpy ( dtyparr[i].bsflag,  " " );
	    strcpy ( dtyparr[i].datadir, " " );
	    strcpy ( dtyparr[i].filext,  " " );
	}

/*
 *	Open the table file.
 */
	ftbl = cfl_tbop ( tblnam, dirsym, iret );
	if ( *iret != 0 ) return;

/*
 *	Set station counter to zero.
 */
	kdtyp = 0;

/*
 *	Read in the next record, check for a comment,
 *	and process valid table entries.
 */
	while ( fgets ( record, 132, ftbl ) != NULL ) {
	    if ( ( record[0] != '!' ) &&
		 ( kdtyp < *maxdtyp ) ) {
		if ( sscanf ( record, "%s %s %s %s %s",
			      dtyparr[kdtyp].datatyp,
			      dtyparr[kdtyp].loctbl,
			      dtyparr[kdtyp].bsflag,
			      dtyparr[kdtyp].datadir, 
			      dtyparr[kdtyp].filext ) > 0 ) {
		    kdtyp++;
		}
	    }
	}

	*ndtyp = kdtyp;

	fclose ( ftbl );
}
