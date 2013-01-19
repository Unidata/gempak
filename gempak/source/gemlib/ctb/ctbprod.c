#include "geminc.h"
#include "gemprm.h"

void ctb_prod ( char *wheel, char *subset, int maxsub, int *nsub, 
		char subout[][5], char descr[][41], int kbit[], 
		int klin[], int krot[], int kind[], int krsv[], 
		int *iret )
/************************************************************************
 * ctb_prod								*
 *									*
 * This subroutine reads the FAX product definition table for the 	*
 * requested product. If the requested subset is "0000", then all 	*
 * subsets for the given wheel number are returned.			*
 *									*
 * ctb_prod ( wheel, subset, maxsub, nsub, subout, descr, kbit, klin,	*
 *	      krot, kind, krsv, iret )					*
 *									*
 * Input parameters:							*
 *	*wheel		char		FAX product wheel ID		*
 *	*subset		char		FAX product subset ID		*
 *	maxsub		int		Max number of subsets		*
 *									*
 * Output parameters:							*
 *	*nsub		int		Number of subsets found		*
 *	subout[nsub][5]	char		FAX product subset IDs		*
 *	descr[nsub][41]	char		FAX product description		*
 *	kbit[nsub]	int		Number of bits per raster line	*
 *	klin[nsub]	int		Number of raster lines		*
 *	krot[nsub]	int		Rotation, in degrees		*
 *	kind[nsub]	int		Number of bits to indent	*
 *	krsv[nsub]	int		Reserved value			*
 *	*iret		int		Return code			*
 *					  -1 = table cannot be opened	*
 *					  -2 = entry not found		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/97						*
 * S. Jacobs/NCEP	 7/97	Made sure that a NULL is added to descr	*
 * S. Jacobs/NCEP	 7/97	Added indent value			*
 * S. Jacobs/NCEP	 8/97	Added reserved value			*
 * S. Jacobs/NCEP	 5/98	Allow return of multiple subsets	*
 ***********************************************************************/
{
	FILE	*ftbl;
	char	tblnam[72], dirsym[72], buffer[133],
		twhl[5], tsub[5], tdsc[41];
	int	ibit, ilin, irot, iind, irsv, i, j, k, ier;

/*---------------------------------------------------------------------*/

/*
 *	Initialize output variables.
 */
	*iret = -2;
	for ( i = 0; i < maxsub; i++ ) {
	    subout[i][0] = CHNULL;
	    descr[i][0]  = CHNULL;
	    kbit[i] = IMISSD;
	    klin[i] = IMISSD;
	    krot[i] = IMISSD;
	    kind[i] = IMISSD;
	    krsv[i] = IMISSD;
	}

/*
 *	Open the table file.
 */
	strcpy ( tblnam, "faxprod.tbl" );
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
	k = 0;
	while ( ( fgets ( buffer, 132, ftbl ) != NULL ) &&
		( k < maxsub ) ) {
	    if  ( buffer[0] != '!' )  {
		if  ( sscanf ( buffer, "%4s %4s %40c %d %d %d %d %d",
			       &twhl[0], &tsub[0], &tdsc[0],
			       &ibit, &ilin, &irot, &iind,
			       &irsv ) >= 8 )  {
		    i = strcmp ( twhl,  wheel );
		    if  ( strcmp ( subset, "0000" ) == 0 )  {
			j = 0;
		    }
		    else {
			j = strcmp ( tsub, subset );
		    }
		    if  ( ( i == 0 ) && ( j == 0 ) )  {
			cst_ncpy ( subout[k], tsub, 4, &ier );
			cst_ncpy ( descr[k], tdsc, 40, &ier );
			kbit[k] = ibit;
			klin[k] = ilin;
			krot[k] = irot;
			kind[k] = iind;
			krsv[k] = irsv;
			*iret = 0;
			k++;
		    }
		}
	    }
	}

	*nsub = k;

	cfl_clos ( ftbl, &ier );
}
