#include "gb2def.h"

void gb2_prob( gribfield *gfld, char *param )
/************************************************************************
 * gb2_prob                                                             *
 *									*
 * This function adds probability info to the GEMPAK parameter string.  *
 *									*
 * gb2_prob( gfld, param )						*
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
 * S. Gilbert/NCEP      10/05   Corrected overwrite error w/ param name *
 * S. Chiswell/Unidata	 4/07	Updated case 2 sprintf format 		*
 * K. Tyle/UAlbany      11/13	Updated case 1 sprintf format 		*
 * S. Jacobs/NCEP	 4/14	Removed case 1 change			*
 ***********************************************************************/
{
    int	prob_type;
    float	lower, upper;
    double    scale,sfact;
    char	cpds[32];
    int	ext_flag, num;

/*---------------------------------------------------------------------*/

    ext_flag = 0;
    cpds[0] = '\0';

    switch ( gfld->ipdtnum ) {

        case  5 :		/* Probability fcst */
        case  9 :		/* Probability fcst */
            ext_flag = 1;
            prob_type = gfld->ipdtmpl[17];
            switch (prob_type) {
                case 0 :
                    scale= (double)(-1.0*gfld->ipdtmpl[18]);
                    sfact= pow((double)10.0,scale);
                    lower = (float)(gfld->ipdtmpl[19]) * sfact;
                    sprintf( cpds, "%04dPB", (int) lower );
                    break;
                case 1 :
                    scale= (double)(-1.0*gfld->ipdtmpl[20]);
                    sfact= pow((double)10.0,scale);
                    upper = (float)(gfld->ipdtmpl[21]) * sfact;
                    sprintf( cpds, "%04dPA", (int) upper );
                    break;
                case 2 :
                    scale= (double)(-1.0*gfld->ipdtmpl[18]);
                    sfact= pow((double)10.0,scale);
                    lower = (float)(gfld->ipdtmpl[19]) * sfact;
                    scale= (double)(-1.0*gfld->ipdtmpl[20]);
                    sfact= pow((double)10.0,scale);
                    upper = (float)(gfld->ipdtmpl[21]) * sfact;
                    sprintf( cpds, "%04d%04d", (int) lower, (int) upper );
                    break;
            }
            break;
    }
    if ( ext_flag == 1 ) {
        num = 12 - strlen(param);
        strncat ( param, cpds, (size_t)num );
    }
    return;
}
