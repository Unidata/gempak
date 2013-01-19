#include "gbcmn.h"

void gb_ecmwfcpc ( unsigned char *buff )
/************************************************************************
 * gb_ecmwfcpc								*
 *									*
 * This function decodes section 1 (Product Definition Section)		*
 * for the Control, Perturbed and Calibtration/Validation forecast	*
 * products of ECMWF Ensemble Prediction System, assuming it contains 	*
 * ensemble forecast data (octet 41 = 30).				*
 *									*
 * gb_ecmwfcpc ( buff )							*
 *									*
 * Input parameters:							*
 *	*buff		unsigned char	PDS buffer			*
 **									*
 * Log:									*
 * M. Li/SAIC		05/07	created					*
 * M. Li/SAIC		06/07	Added case 17 & 18 for byte 43 (type)	*
 ***********************************************************************/
{
int   	indx, defid, class, type, stream, number, fnum;	
char	ver[5], cpds[32];
int	ext_flag, ier;

/*---------------------------------------------------------------------*/

	indx = 40;
	defid = gb_btoi( buff, indx, 1, FALSE );
	indx = 41;
	class = gb_btoi( buff, indx, 1, FALSE );
	indx = 42;
	type = gb_btoi( buff, indx, 1, FALSE );
	indx = 43;
	stream = gb_btoi( buff, indx, 2, FALSE );
	indx = 45;
	cst_ncpy(ver, (const char *)(buff+indx), 4, &ier);
	indx = 49;
        number = gb_btoi( buff, indx, 1, FALSE );
        indx = 50;
        fnum  = gb_btoi( buff, indx, 1, FALSE );

        if ( GBDIAG_PDS == TRUE )  {
            printf(" PDS EXT byte       41 (defid)      = %d\n", defid );
            printf(" PDS EXT byte       42 (class)      = %d\n", class );
            printf(" PDS EXT byte       43 (type)       = %d\n", type );
            printf(" PDS EXT byte    44-45 (stream)     = %d\n", stream);
            printf(" PDS EXT byte    46-49 (ver)        = %s\n", ver);
            printf(" PDS EXT byte       50 (number)     = %d\n", number);
            printf(" PDS EXT byte       51 (fnum)       = %d\n", fnum);
        }

	ext_flag = 0;
	cpds[0] = '\0';

	if ( pds.length > 40 ) {

	    ext_flag = 1;

	    switch ( type ) {

		case 10 :		/* Control forecast */
			 sprintf( cpds, "C%03d", number );
			 break;

		case 11 :		/* Perturbed forecast */
			 sprintf( cpds, "P%03d", number );
                         break;

		case 17 :               /* Ensemble Means  */
                         sprintf( cpds, "M%03d", number );
                         break;

		case 18 :               /* Ensemble standard deviations */
                         sprintf( cpds, "S%03d", number );
                         break;

	    }
	}

	pds.pdse = ext_flag;
	strcpy ( pds.extension, cpds );

	return;
}
