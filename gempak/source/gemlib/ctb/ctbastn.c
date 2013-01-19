#include "geminc.h"
#include "gemprm.h"

void ctb_astn ( char *tblnam, char *dirsym, int *maxstn, int *nstn, 
		char stid[][9], char stnnam[][33], int istnm[], 
		char stat[][3], char coun[][3], float slat[], 
		float slon[], float selv[], int ispri[], 
		char tbchrs[][21], int *iret )
/************************************************************************
 * ctb_astn								*
 *									*
 * This subroutine reads information for all of the stations		*
 * in a GEMPAK station table.						*
 *									*
 * ctb_astn ( tblnam, dirsym, maxstn, nstn, stid, stnnam, istnm, stat,	*
 *	      coun, slat, slon, selv, ispri, tbchrs, iret )		*
 *									*
 * Input parameters:							*
 *	*tblnam		  char		Station table name		*
 *	*dirsym		  char		Directory path/symbol		*
 *	*maxstn		  int		Number of stations to read	*
 *									*
 * Output parameters:							*
 *	*nstn		  int		Number of stations returned	*
 *	stid   [NSTN][9]  char		Station identifier		*
 *	stnnam [NSTN][33] char		Station name			*
 *	istnm  [NSTN]	  int		Station number			*
 *	stat   [NSTN][3]  char		State				*
 *	coun   [NSTN][3]  char	  	Country				*
 *	slat   [NSTN]	  float		Station latitude		*
 *	slon   [NSTN]	  float		Station longitude		*
 *	selv   [NSTN]	  float		Station elevation		*
 *	ispri  [NSTN]	  int		Station priority		*
 *	tbchrs [NSTN][21] char		Additional parameters		*
 *	*iret		  int		Return code			*
 *					  -1 = cannot open table	*
 **									*
 * Log:									*
 * G. Krueger/EAI	 1/97						*
 * G. Krueger/EAI	 3/97	Fix SSCANF read, header, CST_LSTR call	*
 * G. Krueger/EAI	 7/97	Fix SSCANF to read a blank state field	*
 * S. Jacobs/NCEP	 8/98	Fixed state to make it end with a NULL	*
 * S. Jacobs/NCEP	 8/01	Added check for 4-char stid; fixed frmt	*
 * S. Jacobs/NCEP	 9/01	Fixed format for sscanf call		*
 ***********************************************************************/
{
	FILE	*ftbl;
	int	ii, lens, ilat, ilon, ielv, nscan, nid, ier;
	char	buffer[133];

/*---------------------------------------------------------------------*/
	*nstn = 0;
	for ( ii = 0; ii < *maxstn; ii++) {
	    stid[ii][0]   = CHNULL;
	    stnnam[ii][0] = CHNULL; 
	    stat[ii][0]   = CHNULL;
	    coun[ii][0]   = CHNULL;
	    tbchrs[ii][0] = CHNULL;
	}
/*
 *	Open the table file.
 */
	ftbl = cfl_tbop ( tblnam, dirsym, iret );
	if  ( *iret != 0 )  {
	    *iret = -1;
	    return;
	}

	*iret   = 0;

/*
 *	Read in the next record, check for a comment,
 *	and process valid table entries.
 */
	while ( ( fgets ( buffer, 132, ftbl ) != NULL ) && ( *nstn < *maxstn ) )
	{
	    if ( buffer[0] != '!' ) {
/*
 *		Check for 4-char stid or 8-char stid.
 *
 *		Read a record if it has enough fields.
 *
 *		The %*c in SSCANF reads the space between 
 *		the fields and discards it.
 */
		if  ( ( strncmp ( &(buffer[4]), "    ", 4 ) == 0 ) ||
		      ( strncmp ( &(buffer[4]), " ", 1 ) != 0 ) )  {
		    nscan = sscanf ( buffer, "%8c%*c%6d%*c%32c%*c%2c%*c%2c%*c%5d%*c%6d%*c%5d%*c%2d%20c",
			             &stid[*nstn][0], &istnm[*nstn],
			             &stnnam[*nstn][0], &stat[*nstn][0],
			             &coun[*nstn][0], &ilat, &ilon, &ielv,
			             &ispri[*nstn], &tbchrs[*nstn][0] );
		    nid = 8;
		}
		else {
		    nscan = sscanf ( buffer, "%4c%*c%6d%*c%32c%*c%2c%*c%2c%*c%5d%*c%6d%*c%5d%*c%2d%20c",
			             &stid[*nstn][0], &istnm[*nstn],
			             &stnnam[*nstn][0], &stat[*nstn][0],
			             &coun[*nstn][0], &ilat, &ilon, &ielv,
			             &ispri[*nstn], &tbchrs[*nstn][0] );
		    nid = 4;
		}
		if  ( nscan >= 9 )  {
		    stid[*nstn][nid] = CHNULL;
		    cst_lstr ( stid[*nstn], &lens, &ier );
		    stid[*nstn][lens] = CHNULL;

		    stnnam[*nstn][32] = CHNULL;
		    cst_lstr ( stnnam[*nstn], &lens, &ier );
		    stnnam[*nstn][lens] = CHNULL;

		    stat[*nstn][2] = CHNULL;
		    cst_lstr ( stat[*nstn], &lens, &ier );
		    stat[*nstn][lens] = CHNULL;

		    coun[*nstn][2] = CHNULL;
		    cst_lstr ( coun[*nstn], &lens, &ier );
		    coun[*nstn][lens] = CHNULL;

		    tbchrs[*nstn][20] = CHNULL;
		    cst_lstr ( tbchrs[*nstn], &lens, &ier );
		    tbchrs[*nstn][lens] = CHNULL;
		    if  ( tbchrs[*nstn][lens-1] == '\n' )  {
			tbchrs[*nstn][lens-1] = CHNULL;
		    }
		    slat[*nstn] = (float)ilat / 100.0F;
		    slon[*nstn] = (float)ilon / 100.0F;
		    selv[*nstn] = (float)ielv;
		    (*nstn)++;
		}
	    }
	}

	cfl_clos ( ftbl, &ier );
}
