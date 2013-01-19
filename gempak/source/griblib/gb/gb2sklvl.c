#include "gb2def.h"

void  gb2_sklvl( int lvl1, int lvl2, G2lvls *lvltbl,
                 G2level *g2lev, int *iret)
/************************************************************************
 * gb2_sklvl	            						*
 *									*
 * This function searches a GRIB2 level/layer table from a              *
 * G2level structer, and returns a structure containing the table        *
 * elements for the entry that matches the two level values given in    *
 * lvl1 and lvl2.                                                       *
 *                                                                      *
 * gb2_sklvl ( lvl1, lvl2, lvltbl, g2lev, iret )      	        	*
 *									*
 * Input parameters:							*
 *	lvl1		int		1st GRIB2 level id              *
 *	lvl2		int		2nd GRIB2 level id              *
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
 * S. Gilbert/NCEP		 12/2004				*
 ***********************************************************************/
{

    int n;

/*---------------------------------------------------------------------*/
    *iret = -1;

    n=0;
    while ( n < lvltbl->nlines ) {

        if ( lvl1 == lvltbl->info[n].id1 &&
             lvl2 == lvltbl->info[n].id2 ) {

            g2lev->id1=lvltbl->info[n].id1;
            g2lev->id2=lvltbl->info[n].id2;
            strcpy( g2lev->name, lvltbl->info[n].name );
            strcpy( g2lev->unit, lvltbl->info[n].unit );
            strcpy( g2lev->abbrev, lvltbl->info[n].abbrev );
            g2lev->scale=lvltbl->info[n].scale;
            *iret=0;
            break;
        }
        n++;
    }

}
