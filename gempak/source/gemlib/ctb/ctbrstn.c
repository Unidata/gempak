#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

void ctb_rstn ( const char *tblnam, const char *tbltyp, 
		int *nstn, StnLst **stnarr, int *iret )
/************************************************************************
 * ctb_rstn								*
 *									*
 * This routine will read a station table into an array of structures.	*
 *									*
 * ctb_rstn ( tblnam, tbltyp, nstn, stnarr, iret )			*
 *									*
 * Input parameters:							*
 *	*tblnam		const char	Station table name		*
 *	*tbltyp		const char	Table type (subdirectory)	*
 *									*
 * Output parameters:							*
 *	*nstn		int		Number of stations		*
 *	*stnarr		StnLst		Station list structure		*
 *	*iret		int		Return code			*
 *					   As for cfl_ropn		*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 6/94						*
 * L. Williams/EAI	 7/94		Reformat header			*
 * G. Krueger/EAI	 3/96		CFL_SOPN -> CFL_ROPN		*
 * T. Piper/GSC		 3/99		Corrected prolog		*
 * H. Zeng/XTRIA	10/03           Added misc_info			*
 * T. Piper/SAIC	05/04	Replaced cfl_ropn with cfl_tbop         *
 * T. Piper/SAIC	09/07	Dynamically allocate station table array*
 ***********************************************************************/
{
    FILE    *ftbl;
    char    record[133];
    int	    ilat, ilon, ielv;
    StnLst  *tmparr=NULL;
/*---------------------------------------------------------------------*/
/*
 *  Open the table file.
 */
    ftbl = cfl_tbop ( (char*)tblnam, (char*)tbltyp, iret );
    if ( *iret != 0 ) return;

/*
 *  Set station counter to zero.
 */
    *nstn = 0;

/*
 *  Read in the next record, check for a comment,
 *  and process valid table entries.
 */
    while ( fgets ( record, 132, ftbl ) != NULL ) {
	if ( record[0] != '!' ) {
	    G_REALLOC(tmparr, StnLst, (*nstn)+1, "ctb_rstn:  stnarr");
	    if ( sscanf ( record, "%s %d %s %s %s %d %d %d %d %s",
		       tmparr[*nstn].stid, &tmparr[*nstn].stnm,
		       tmparr[*nstn].name,  tmparr[*nstn].state,
		       tmparr[*nstn].coun, &ilat, &ilon, &ielv,
		      &tmparr[*nstn].prior,
		       tmparr[*nstn].misc_info          ) > 0 ) {
		tmparr[*nstn].rlat = ( (float) ilat ) / 100.0F;
		tmparr[*nstn].rlon = ( (float) ilon ) / 100.0F;
		tmparr[*nstn].elev = (float) ielv;
		(*nstn)++;
	    }
        }
    }
    fclose ( ftbl );
    *stnarr = tmparr;
    *iret = 0;
}
