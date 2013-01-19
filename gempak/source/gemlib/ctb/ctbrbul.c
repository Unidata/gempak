#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

void ctb_rbul ( char *tblnam, char *tbltyp, int *maxbul, int *nbul, 
				struct bulletin_list *bularr, int *iret )
/************************************************************************
 * ctb_rbul								*
 *									*
 * This routine will read a bulletin table into an array of structures.	*
 *									*
 * ctb_rbul ( tblnam, tbltyp, maxbul, nbul, bularr, iret )		*
 *									*
 * Input parameters:							*
 *	*tblnam		char		Bulletin table name		*
 *	*tbltyp		char		Table type (subdirectory)	*
 *	*maxbul		int		Maximum number of bulletins	*
 *									*
 * Output parameters:							*
 *	*nbul		int		     Number of bulletins	*
 *	*bularr		struct bulletin_list Bulletin list structure	*
 *	*iret		int		     Return code		*
 *					   As for cfl_ropn		*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 6/94						*
 * L. Williams/EAI	 7/94		Reformat header			*
 * G. Krueger/EAI	 3/96		CFL_SOPN -> CFL_ROPN		*
 * T. Piper/SAIC	05/04	Replaced cfl_ropn with cfl_tbop		*
 ***********************************************************************/
{
	FILE    *ftbl;
	char    record[133];
	int	ii, kstn, ilat, ilon, ielv;

/*---------------------------------------------------------------------*/
	*iret = 0;

/*
 *	Initialize the structure values.
 */
	for ( ii = 0; ii < *maxbul; ii++ ) {
	    strcpy ( bularr[ii].bullid, " " );
	    strcpy ( bularr[ii].stid,   " " );
	    strcpy ( bularr[ii].name,   " " );
	    strcpy ( bularr[ii].state,  " " );
	    strcpy ( bularr[ii].coun,   " " );
	    bularr[ii].rlat = 0.0F;
	    bularr[ii].rlon = 0.0F;
	    bularr[ii].elev = 0.0F;
	}

/*
 *	Open the table file.
 */
	ftbl = cfl_tbop ( tblnam, tbltyp, iret );
	if ( *iret != 0 ) return;

/*
 *	Set station counter to zero.
 */
	kstn = 0;

/*
 *	Read in the next record, check for a comment,
 *	and process valid table entries.
 */
	while ( fgets ( record, 132, ftbl ) != NULL ) {
	    if ( ( record[0] != '!' ) &&
		 ( kstn < *maxbul ) ) {
		if ( sscanf ( record, "%s %s %s %s %s %d %d %d",
			       bularr[kstn].bullid, bularr[kstn].stid,
			       bularr[kstn].name,   bularr[kstn].state,
			       bularr[kstn].coun,
			      &ilat, &ilon, &ielv ) > 0 ) {
		    bularr[kstn].rlat = ( (float) ilat ) / 100.0F;
		    bularr[kstn].rlon = ( (float) ilon ) / 100.0F;
		    bularr[kstn].elev = (float) ielv;
		    kstn++;
		}
	    }
	}

	*nbul = kstn;

	fclose ( ftbl );
}
