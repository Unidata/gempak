#include "gbcmn.h"

void gb_ensemble ( unsigned char *buff )
/************************************************************************
 * gb_ensemble								*
 *									*
 * This function decodes section 1 (Product Definition Section)		*
 * assuming it contains an ensemble extension (octet 41 = 1).		*
 * Entries are made or corrected in the global PDS structure.		*
 *									*
 * gb_ensemble ( buff )							*
 *									*
 * Input parameters:							*
 *	*buff		unsigned char	PDS buffer			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/96	New					*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 * S. Jacobs/NCEP	 5/06	Fixed format for lower/upper addition	*
 ***********************************************************************/
{
int	indx;
char	ctmp[32];
int	appl, type, idnum, product, smooth, prob_type, prob_def;
int	first_byte, isign, ia, ib;
float	lower, upper;

char	cpds[32];
int	ext_flag;

	indx = 40;
	appl = gb_btoi( buff, indx, 1, FALSE );
	indx = 41;
	type = gb_btoi( buff, indx, 1, FALSE );
	indx = 42;
	idnum = gb_btoi( buff, indx, 1, FALSE );
	indx = 43;
	product = gb_btoi( buff, indx, 1, FALSE );
	indx = 44;
	smooth = gb_btoi( buff, indx, 1, FALSE );

        if ( GBDIAG_PDS == TRUE )  {
            printf(" PDS EXT byte       41 (appl)      = %d\n", appl );
            printf(" PDS EXT byte       42 (type)      = %d\n", type );
            printf(" PDS EXT byte       43 (idnum)     = %d\n", idnum );
            printf(" PDS EXT byte       44 (product)   = %d\n", product );
            printf(" PDS EXT byte       45 (smooth)    = %d\n", smooth );
        }

	ext_flag = 0;
	cpds[0] = '\0';

	if ( pds.length <= 45 || ( pds.length > 45 &&
		pds.parameter != 191 && pds.parameter != 192 ) )  {	

		/* 
		 * Normal processing 
		 */

		ext_flag = 1;

		switch ( type ) {
	
		case 1 :		/* Unperturbed control fcst */
			sprintf( cpds, "C%03d", idnum );
			break;

		case 2 :		/* Individual neg perturbed fcst */
			sprintf( cpds, "N%03d", idnum );
			break;

		case 3 :		/* Individual pos perturbed fcst */
			sprintf( cpds, "P%03d", idnum );
			break;

		case 4 :		/* Cluster */
			switch ( product ) {
			case 1 :		/* Unweighted mean */
				strcpy ( ctmp, "CM" );
				break;
			case 2 :		/* Weighted mean */
				strcpy ( ctmp, "WM" );
				break;
			case 3 :		/* Unweighted median */
				strcpy ( ctmp, "CH" );
				break;
			case 4 :		/* Weighted median */
				strcpy ( ctmp, "WH" );
				break;
			case 11 :		/* Stddev wrt ens mean */
				strcpy ( ctmp, "CS" );
				break;
			case 12 :		/* Stddev wrt ens mean, nrml */
				strcpy ( ctmp, "CN" );
				break;
			case 21 :		/* Stddev wrt ens wmean */
				strcpy ( ctmp, "WS" );
				break;
			case 22 :		/* Stddev wrt ens wmean,nrml */
				strcpy ( ctmp, "WN" );
				break;
			case 31 :		/* Stddev wrt ens median */
				strcpy ( ctmp, "CD" );
				break;
			case 32 :		/* Stddev wrt ens median,nrml */
				strcpy ( ctmp, "CR" );
				break;
			case 41 :		/* Stddev wrt ens wmedian */
				strcpy ( ctmp, "WD" );
				break;
			case 42 :		/* Stddev wrt ens wmedian,nrml*/
				strcpy ( ctmp, "WR" );
				break;
			}
			sprintf( cpds, "%s%02d", ctmp, idnum );
			break;

		case 5 :		/* Whole ensemble */
			strcpy ( cpds, "EN" );
			switch ( product ) {
			case 1 :		/* Unweighted mean */
				strcat ( cpds, "MA" );
				break;
			case 2 :		/* Weighted mean */
				strcat ( cpds, "MW" );
				break;
			case 3 :		/* Unweighted median */
				strcat ( cpds, "HA" );
				break;
			case 4 :		/* Weighted median */
				strcat ( cpds, "HW" );
				break;
			case 11 :		/* Stddev wrt ens mean */
				strcat ( cpds, "SA" );
				break;
			case 12 :		/* Stddev wrt ens mean, nrml */
				strcat ( cpds, "NA" );
				break;
			case 21 :		/* Stddev wrt ens wmean */
				strcat ( cpds, "SW" );
				break;
			case 22 :		/* Stddev wrt ens wmean,nrml */
				strcat ( cpds, "NW" );
				break;
			case 31 :		/* Stddev wrt ens median */
				strcat ( cpds, "DA" );
				break;
			case 32 :		/* Stddev wrt ens median,nrml */
				strcat ( cpds, "RA" );
				break;
			case 41 :		/* Stddev wrt ens wmedian */
				strcat ( cpds, "DW" );
				break;
			case 42 :		/* Stddev wrt ens wmedian,nrml*/
				strcat ( cpds, "RW" );
				break;
			}
			break;

		}
 
	}

	else if ( pds.length > 45 &&
			( pds.parameter == 191 || pds.parameter == 192 ) )  {

		/* 
		 * Probability processing 
		 */

		indx = 45;
		prob_def = gb_btoi( buff, indx, 1, FALSE );
		indx = 46;
		prob_type = gb_btoi( buff, indx, 1, FALSE );

	        ext_flag = 1;

		/*
        	 * BYTES 48-51, 52-55
        	 * Limit values. The limit values are floating point
        	 * numbers constructed from various parts of these 
		 * two pairs of four bytes. First lower limit then upper limit.
		 */

		indx = 47;
        	first_byte = gb_btoi( buff, indx, 1, FALSE );
        	isign = first_byte >> 7;
        	ia = first_byte & 127;
		indx = 48;
        	ib = gb_btoi( buff, indx, 3, FALSE );

        	lower = (float) ( pow(-1.0,(double)isign) *
                         pow(2.0,-24.0) * (double) ib *
                         pow(16.0,(double)ia-64.0) );

		indx = 51;
                first_byte = gb_btoi( buff, indx, 1, FALSE );
                isign = first_byte >> 7;
                ia = first_byte & 127;
                indx = 52;
                ib = gb_btoi( buff, indx, 3, FALSE );

                upper = (float) ( pow(-1.0,(double)isign) *
                         pow(2.0,-24.0) * (double) ib *
                         pow(16.0,(double)ia-64.0) );

		/* 	
		 *	Check for lower, upper or range.
		 */

		switch ( prob_type ) {
		case 1 :
			sprintf( cpds, "%04dPB", (int) lower );
			break;
		case 2 :
			sprintf( cpds, "%04dPA", (int) upper );
			break;
		case 3 :
			sprintf( cpds, "%04d%04d", (int) lower, (int) upper );
			break;
		}

		if ( type == 4 ) {

			/* 	
			 *	Check if probability is for cluster.
			 */

			sprintf( ctmp, "%02d", idnum );
                        strcat ( cpds, ctmp );
		}
		else if ( pds.time_range == 5 ) {

			/* 	
			 *	Check tendency.
			 */

                        strcat ( cpds, "TN" );
		}

		/*
		 *	Switch octet 46 info into pds octet 9 slot.
		 */

		pds.parameter = prob_def;

        	if ( GBDIAG_PDS == TRUE )  {
		    printf(" PDS EXT byte       46 (prob_def)  = %d\n", prob_def );
		    printf(" PDS EXT byte       47 (prob_type) = %d\n", prob_type );
		    printf(" PDS EXT bytes  48- 51 (lower)     = %04d\n", (int) lower );
		    printf(" PDS EXT bytes  52- 55 (upper)     = %04d\n", (int) upper );
		    printf(" PDS byte      9 (pds.parameter)   = %d\n", pds.parameter );
		}

	}

	pds.pdse = ext_flag;
	strcpy ( pds.extension, cpds );

	return;

}
