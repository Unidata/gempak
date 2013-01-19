#include "gb2def.h"

void  gb2_gtlclvartbl( char *lclvartbl, char *cntr, int lclver,
                       G2vars_t **g2vartbl, int *iret)
/************************************************************************
 * gb2_gtlclvartbl						        *
 *									*
 * This function reads the Local GRIB2 paramter tables from             *
 * specified file and returns a structure containing the table          *
 * entries.                                                             *
 *                                                                      *
 * If lclvartbl is NULL, the default table is read.                     *
 *									*
 * gb2_gtlclvartbl ( lclvartbl, cntr, lclver, g2vartbl, iret )          *
 *									*
 * Input parameters:							*
 *      *lclvartbl      char            Local GRIB2 Parameter table     *
 *                                             filename                 *
 *      *cntr           char            Abbrev for Orig Center          *
 *      lclver            int           Local Table version number      *
 *									*
 * Output parameters:							*
 *      **g2vartbl      G2vars_t        structure for the table entries *
 *	*iret		int		Return code			*
 *                                        -31 = Error reading table     *
 **									*
 * Log:									*
 * S. Gilbert/NCEP		 08/2005				*
 ***********************************************************************/
{

    char tmpname[LLMXLN];
    int  ier;
    static char currtable[LLMXLN];
    static G2vars_t currvartbl={0,0};

/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Check if user supplied table.  If not, use default.
     */
    if ( strlen(lclvartbl) == (size_t)0 ) {
        sprintf( tmpname,"g2vars%s%d.tbl", cntr, lclver );
    }
    else {
        strcpy( tmpname, lclvartbl );
    }

    /*
     *  Check if table has already been read in. 
     *  If different table, read new one in.
     */
    if ( strcmp( tmpname, currtable ) != 0 ) {
        if ( currvartbl.info != 0 ) {
            free(currvartbl.info);
            currvartbl.info=0;
            currvartbl.nlines=0;
        }
        printf(" Opening Local GRIB2 Parameter Table %s...\n",tmpname);
        ctb_g2rdvar( tmpname, &currvartbl, &ier );
        if ( ier != 0 ) {
            currvartbl.nlines=0;
            *iret=-31;
            er_wmsg("GB",iret,tmpname,&ier,2,strlen(tmpname));
            *g2vartbl = &currvartbl;
            return;
        }
    }
    strcpy( currtable, tmpname );
    *g2vartbl = &currvartbl;

    /*
     *  Search through table for id.
    gb2_skvar( disc, cat, id, pdtn, &vartbl, g2var, &ier);
    if ( ier == -1 )*iret=-32;
     */

}
