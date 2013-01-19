#include "geminc.h"
#include "gemprm.h"

void ctb_afos ( char *pil, char *descr, int *kx, int *ky, 
			int *kmap, int *ktime, int *kgscl, int *iret )
/************************************************************************
 * ctb_afos								*
 *									*
 * This subroutine reads the AFOS product definition table for the 	*
 * requested product.							*
 *									*
 * ctb_afos ( pil, descr, kx, ky, kmap, ktime, kgscl, iret )		*
 *									*
 * Input parameters:							*
 *	*pil		char		AFOS PIL			*
 *									*
 * Output parameters:							*
 *	*descr		char		FAX product description		*
 *	*kx		int		X coordinate			*
 *	*ky		int		Y coordinate			*
 *	*kmap		int		AFOS map id			*
 *	*ktime		int		Valid time increment		*
 *	*kgscl		int		Geographic scale factor		*
 *	*iret		int		Return code			*
 *					  -1 = table cannot be opened	*
 *					  -2 = entry not found		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 8/97	Copied from CTB_PROD			*
 * T. Piper/SAIC	 4/02	Fixed UMR; reduced len in cst_ncpy to 32*
 ***********************************************************************/
{

	FILE	*ftbl;
	char	tblnam[72], dirsym[72], buffer[133],
		tpil[4], tdsc[33];
	int	found, ix, iy, imap, itime, igscl, i, ier;

/*---------------------------------------------------------------------*/

/*
 *	Initialize output variables.
 */
	*iret  = -2;
	*kx    = IMISSD;
	*ky    = IMISSD;
	*kmap  = IMISSD;
	*ktime = IMISSD;
	*kgscl = IMISSD;
	descr[0] = CHNULL;

/*
 *	Open the table file.
 */
	strcpy ( tblnam, "afosprod.tbl" );
	strcpy ( dirsym, "pgen" );
	ftbl = cfl_tbop ( tblnam, dirsym, &ier );
	if  ( ier != 0 )  {
	    *iret = -1;
	    return;
	}

/*
 *	Read in the next record, check for a comment,
 *	and process valid table entries.
 */
	found = G_FALSE;
	while ( ( fgets ( buffer, 132, ftbl ) != NULL ) && ( ! found ) )
	{
	    if  ( buffer[0] != '!' )  {
		if  ( sscanf ( buffer, "%4s %32c %d %d %d %d %d",
			       &tpil[0], &tdsc[0],
			       &ix, &iy, &imap, &itime,
			       &igscl ) >= 7 )  {
		    i = strcmp ( tpil,  pil );
		    if  ( i == 0 )  {
			found = G_TRUE;
			cst_ncpy ( descr, tdsc, 32, &ier );
			*kx    = ix;
			*ky    = iy;
			*kmap  = imap;
			*ktime = itime;
			*kgscl = igscl;
			*iret  = 0;
		    }
		}
	    }
	}

	cfl_clos ( ftbl, &ier );
}
