#include "gbcmn.h"

void gb_ecmwfclu ( unsigned char *buff )
/************************************************************************
 * gb_ecmwfclu								*
 *									*
 * This function decodes section 1 (Product Definition Section)		*
 * for Cluster Mean and Cluster Standard Deviation products of		*
 * ECMWF Ensemble Prediction System.					*
 *									*
 * gb_ecmwfclu ( buff )							*
 *									*
 * Input parameters:							*
 *	*buff		unsigned char	PDS buffer			*
 **									*
 * Log:									*
 * M. Li/SAIC		06/07	created					*
 ***********************************************************************/
{
int   	indx, defid, class, type, stream, number, tnum;	
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
	cst_ncpy(ver, (const char *)(buff+indx), 4, &ier );
	indx = 49;
        number = gb_btoi( buff, indx, 1, FALSE );
        indx = 50;
        tnum  = gb_btoi( buff, indx, 1, FALSE );

        if ( GBDIAG_PDS == TRUE )  {
            printf(" PDS EXT byte       41 (defid)          = %d\n", defid );
            printf(" PDS EXT byte       42 (class)          = %d\n", class );
            printf(" PDS EXT byte       43 (type)           = %d\n", type );
            printf(" PDS EXT byte    44-45 (stream)         = %d\n", stream);
            printf(" PDS EXT byte    46-49 (ver)            = %s\n", ver);
            printf(" PDS EXT byte       50 (Cluster number) = %d\n", number);
            printf(" PDS EXT byte       51 (total_num)      = %d\n", tnum);
        }

	ext_flag = 0;
	cpds[0] = '\0';

	if ( pds.length > 40 ) {

	    ext_flag = 1;

	    switch ( type ) {

		case 14 :		/* Cluster Means */
			 sprintf( cpds, "CM%02d", number );
			 break;

		case 15 :		/* Cluster standard deviations */
			 sprintf( cpds, "CS%02d", number );
                         break;

	    }
	}

	pds.pdse = ext_flag;
	strcpy ( pds.extension, cpds );

	return;
}
