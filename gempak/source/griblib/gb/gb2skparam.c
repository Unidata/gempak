#include "gb2def.h"

void  gb2_skparam( char *param , G2vars_t *vartbl,
                 G2Vinfo *g2var, int *iret)
/************************************************************************
 * gb2_skparam	            						*
 *									*
 * This function searches a GRIB2 parameter table from a                *
 * G2vars_t structure, and returns a structure containing the table     *
 * elements for the entry that matches the three identifying parameter  *
 * numbers and the PDT TEMPLATE number.                                 *
 *                                                                      *
 * gb2_skparam ( param, vartbl, g2var, iret )                    	*
 *									*
 * Input parameters:							*
 *      param            char           Parameter name                  *
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

        if ( strncmp( param, vartbl->info[n].gemname, strlen(param) ) == 0 ) {

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
