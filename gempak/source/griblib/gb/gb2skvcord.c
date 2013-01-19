#include "gb2def.h"

void  gb2_skvcord( int vcord, G2lvls *lvltbl,
                 G2level *g2lev, int *iret)
/************************************************************************
 * gb2_skvcord	            						*
 *									*
 * This function searches a GRIB2 level/layer table from a              *
 * G2level structure and returns a structure containing the table       *
 * elements for the entry that matches the vertical coordinate          *
 * abbreviation in vcord.                                               *
 *                                                                      *
 * gb2_skvcord ( vcord, lvltbl, g2lev, iret )      	        	*
 *									*
 * Input parameters:							*
 *	vcord		int		1st GRIB2 level id              *
 *	*lvltbl		G2lvls		structure containing GRIB2      *
 *                                      level table entries read from   *
 *                                      a table file.                   *
 *									*
 * Output parameters:							*
 *	*g2lev		G2level		structure for level table entry *
 *	*iret		int		Return code			*
 *                                        0 = entry found               *
 *                                       -1 = entry NOT found           *
 **									*
 * Log:									*
 * S. Gilbert/NCEP		 08/2005				*
 ***********************************************************************/
{

    int    n, ier;
    char   abbrev[5];

/*---------------------------------------------------------------------*/
    *iret = -1;

    lv_ccrd( &vcord, abbrev, &ier, 4 );
    abbrev[4] = '\0';

    n=0;
    while ( n < lvltbl->nlines ) {

        if ( strncmp( abbrev, lvltbl->info[n].abbrev, 4 ) == 0 ) {

            g2lev->id1=lvltbl->info[n].id1;
            g2lev->id2=lvltbl->info[n].id2;
            strcpy( g2lev->name, lvltbl->info[n].name );
            strcpy( g2lev->unit, lvltbl->info[n].unit );
            /*strcpy( g2lev->abbrev, lvltbl->info[n].abbrev );*/
            g2lev->scale=lvltbl->info[n].scale;
            *iret=0;
            break;
        }
        n++;
    }

}
