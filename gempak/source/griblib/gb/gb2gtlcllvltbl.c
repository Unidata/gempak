#include "gb2def.h"

void  gb2_gtlcllvltbl( char *lcllvltbl, char *cntr, int lclver,
                       G2lvls **g2levtbl, int *iret)
/************************************************************************
 * gb2_gtlcllvltbl							*
 *									*
 * This function reads the Local GRIB2 level/layer table from           *
 * specified file and returns a structure containing the table          *
 * entries.                                                             *
 *                                                                      *
 * If lcllvltbl is NULL, the default table is read.                     *
 *									*
 * gb2_gtlcllvltbl ( lcllvltbl, cntr, lclver, g2levtbl, iret )	        *
 *									*
 * Input parameters:							*
 *      *lcllvltbl      char            Local vertical coordinate table *
 *      *cntr           char            Abbrev for Orig Center          *
 *      lclver          int             Local Table Version number      *
 *									*
 * Output parameters:							*
 *	**g2levtbl	G2lvls		struct for level table entries  *
 *	*iret		int		Return code			*
 *                                        -29 = Error reading table     *
 **									*
 * Log:									*
 * S. Gilbert/NCEP		 08/2005				*
 ***********************************************************************/
{

    char tmpname[LLMXLN];
    int  ier;
    static char currtable[LLMXLN];
    static G2lvls currlvltbl={0,0};

/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Check if user supplied table.  If not, use default.
     */
    if ( strlen(lcllvltbl) == (size_t)0 ) {
        sprintf( tmpname,"g2vcrd%s%d.tbl", cntr, lclver );
    }
    else {
        strcpy( tmpname, lcllvltbl );
    }

    /*
     *  Check if table has already been read in. 
     *  If different table, read new one in.
     */
    if ( strcmp( tmpname, currtable ) != 0 ) {
        if ( currlvltbl.info != 0 ) {
            free(currlvltbl.info);
            currlvltbl.info=0;
            currlvltbl.nlines=0;
        }
        printf(" Opening Local GRIB2 Vertical Coordinate Table %s...\n",tmpname);
        ctb_g2rdlvl( tmpname, &currlvltbl, &ier );
        if ( ier != 0 ) {
            currlvltbl.nlines=0;
            *iret=-29;
            er_wmsg("GB",iret,tmpname,&ier,2,strlen(tmpname));
            *g2levtbl = &currlvltbl;
            return;
        }
    }
    strcpy( currtable, tmpname );
    *g2levtbl = &currlvltbl;

}
