#include "gb2def.h"

void gb2_ens( gribfield *gfld, char *param )
/************************************************************************
 * gb2_ens                                                              *
 *									*
 * This function adds ensemble info to the GEMPAK parameter string.     *
 *									*
 * gb2_ens( gfld, param )						*
 *									*
 * Input parameters:							*
 *	*gfld		struct gribfield	Decoded GRIB2 structure *
 *									*
 * Input/Output parameters:						*
 *      *param          char            GEMPAK Parameter string         *
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/96	New					*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 * S. Gilbert/NCEP      12/04   Modified from gbensemble.c for use w/   *
 *                              GRIB2                                   *
 * S. Chiswell/Unidata  10/06   Changed strncpy to sprint for C001/2    *
 * S. Gilbert/NCEP      11/07   Corrected strcpy to strcat		*
 ***********************************************************************/
{
        int	type, idnum, product, clnum;
        char	cpds[32];
        int	ext_flag;


	ext_flag = 0;
	cpds[0] = '\0';

        switch ( gfld->ipdtnum ) {
            case  1 :		/* individual ensemble fcst */
            case 11 :		/* individual ensemble fcst */
	        ext_flag = 1;
                type = gfld->ipdtmpl[15];
                idnum = gfld->ipdtmpl[16];
                switch ( type ) {
                    case 0:       /* Unperturbed control fcst - high res  */
                        sprintf( cpds, "C001" );
                        break;
                    case 1:       /* Unperturbed control fcst  - low res */
                        sprintf( cpds, "C002" );
                        break;
                    case 2:       /* Individual neg perturbed fcst */
                        sprintf( cpds, "N%03d", idnum );
                        break;
                    case 3:       /* Individual pos perturbed fcst */
                        sprintf( cpds, "P%03d", idnum );
                        break;
                }
                break;

            case  2 :		/* Whole ensemble */
            case 12 :		/* Whole ensemble */
	        ext_flag = 1;
                product = gfld->ipdtmpl[15];
                strcpy ( cpds, "EN" );
                switch (product) {
                    case 0:       /* Unweighted mean */
                        strcat( cpds, "MA");
                        break;
                    case 1:       /* Weighted mean */
                        strcat( cpds, "MW");
                        break;
                    case 2:       /* Stddev wrt ens mean */
                        strcat( cpds, "SA");
                        break;
                    case 3:       /* Stddev wrt ens mean, nrml */
                        strcat( cpds, "NA");
                        break;
                }
                break;

            case  3 :		/* Cluster */
            case  4 :		/* Cluster */
            case 13 :		/* Cluster */
            case 14 :		/* Cluster */
	        ext_flag = 1;
                product = gfld->ipdtmpl[15];
                clnum = gfld->ipdtmpl[17];
                switch (product) {
                    case 0:       /* Unweighted mean */
                        sprintf( cpds, "CM%02d", clnum);
                        break;
                    case 1:       /* Weighted mean */
                        sprintf( cpds, "WM%02d", clnum);
                        break;
                    case 2:       /* Stddev wrt ens mean */
                        sprintf( cpds, "CS%02d", clnum);
                        break;
                    case 3:       /* Stddev wrt ens mean, nrml */
                        sprintf( cpds, "CN%02d", clnum);
                        break;
                }
                break;

        }

	if ( ext_flag == 1 ) {
	    strcat ( param, cpds );
        }

	return;

}
