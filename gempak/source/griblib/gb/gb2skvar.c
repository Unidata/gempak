#include "gb2def.h"

void  gb2_skvar( int disc, int cat, int id, int pdtn, G2vars_t *vartbl,
                 G2Vinfo *g2var, int *iret)
/************************************************************************
 * gb2_skvar	            						*
 *									*
 * This function searches a GRIB2 parameter table from a                *
 * G2vars_t structure, and returns a structure containing the table     *
 * elements for the entry that matches the three identifying parameter  *
 * numbers and the PDT TEMPLATE number.                                 *
 *                                                                      *
 * gb2_skvar ( disc, cat, id, pdtn, vartbl, g2var, iret )           	*
 *									*
 * Input parameters:							*
 *      disc            int             GRIB2 discipline number         *
 *      cat             int             GRIB2 parameter category        *
 *      id              int             GRIB2 parameter id number       *
 *      pdtn            int             GRIB2 PDT Template number       *
 *	*vartbl		G2vars_t 	structure containing GRIB2      *
 *                                      parameter table entries read    *
 *                                        from a table file.            *
 *									*
 * Output parameters:							*
 *	*g2var		G2Vinfo		structure for parameter table   *
 *                                            entry                     *
 *	*iret		int		Return code			*
 *                                        0 = entry found               *
 *                                       -1 = entry NOT found           *
 **									*
 * Log:									*
 * S. Gilbert/NCEP		 12/2004				*
 ***********************************************************************/
{

    int n;

/*---------------------------------------------------------------------*/
    *iret = -1;

    n=0;
    while ( n < vartbl->nlines ) {

        if ( disc == vartbl->info[n].discpln  &&
             cat  == vartbl->info[n].categry  &&
             id   == vartbl->info[n].paramtr  &&
             pdtn == vartbl->info[n].pdtnmbr ) {

            g2var->discpln=vartbl->info[n].discpln;
            g2var->categry=vartbl->info[n].categry;
            g2var->paramtr=vartbl->info[n].paramtr;
            g2var->pdtnmbr=vartbl->info[n].pdtnmbr;
            strcpy( g2var->name, vartbl->info[n].name );
            strcpy( g2var->units, vartbl->info[n].units );
            strcpy( g2var->gemname, vartbl->info[n].gemname );
            g2var->scale=vartbl->info[n].scale;
            g2var->missing=vartbl->info[n].missing;
            g2var->hzremap=vartbl->info[n].hzremap;
            g2var->direction=vartbl->info[n].direction;
            *iret=0;
            break;
        }
        n++;
    }

}
