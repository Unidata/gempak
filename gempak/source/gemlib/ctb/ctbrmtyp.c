#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

void ctb_rmtyp ( char *tblnam, char *dirsym, int *maxmtyp, 
		 int *nmtyp, struct maptype_list *mtyparr, int *iret )
/************************************************************************
 * ctb_rmtyp								*
 *									*
 * This routine will read a map projection table into an array of	*
 * structures.								*
 *									*
 * ctb_rmtyp ( tblnam, dirsym, maxmtyp, nmtyp, mtyparr, iret )		*
 *									*
 * Input parameters:							*
 *	*tblnam		char		Map type table name		*
 *	*dirsym		char		Directory path/symbol		*
 *	*maxmtyp	int		Maximum number of entries	*
 *									*
 * Output parameters:							*
 *	*nmtyp		int		Number of entries		*
 *	*mtyparr	struct maptype_list Map type list structure	*
 *	*iret		int		Return code			*
 *					   As for cfl_ropn		*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * G. Krueger/EAI	 3/96	CFL_WOPN -> CFL_ROPN		        *
 * C. Lin/EAI	         2/97	only read name and garea from table,    *
 *				proj is set to "DEF"  		        *
 * C. Lin/EAI	         2/97	use cfl_trln, change while logic        *
 * T. Piper/SAIC	01/04	replaced cfl_ropn with cfl_tbop		*
 ***********************************************************************/
{
FILE    *ftbl;
char    record[133];
int	kmtyp, ier;

/*---------------------------------------------------------------------*/
	*iret = 0;

	/*
 	 * Open the table file.
 	 */
	ftbl = cfl_tbop ( tblnam, dirsym, iret );
	if ( *iret != 0 ) return;

	/*
 	 * Set station counter to zero.
 	 */
	kmtyp = 0;

	/*
 	 * Read in the next record, check for a comment,
 	 * and process valid table entries.
 	 */
	while ( !feof(ftbl) && (kmtyp < *maxmtyp) ) {

		/*
		 * read a record
	 	 */
		cfl_trln(ftbl, 132, record, &ier);

		/*
	 	 * parse the record
		 */
		if ( ier == 0 ) {
		    sscanf(record, "%s %s", mtyparr[kmtyp].name,
			      		    mtyparr[kmtyp].garea);
		    strcpy(mtyparr[kmtyp].proj, "DEF");
		    kmtyp++;
		}

	}

	*nmtyp = kmtyp;

	fclose ( ftbl );

}
