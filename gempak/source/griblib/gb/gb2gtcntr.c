#include "gb2def.h"

void  gb2_gtcntr( int cntrid, char *tblnam, char *wmocntr, int *iret)
/************************************************************************
 * gb2_gtcntr								*
 *									*
 * This function reads the WMO originating center table from the        *
 * specified file, and returns the Abbreviation of the Center with      *
 * id = cntrid.                                                         *
 * If the input table name is NULL, the default file, wmocenter.tbl,    *
 * will be read.                                                        *
 *									*
 * gb2_gtcntr ( cntrid, tblnam, wmocntr, iret )				*
 *									*
 * Input parameters:							*
 *	cntrid		int		Originating Center id           *
 *	*tblnam		char		Orig Center table name          *
 *									*
 * Output parameters:							*
 *	*wmocntr	char		Abbreviation of Orig Center     *
 *	*iret		int		Return code			*
 *                                        -13 = Error reading table    *
 *                                        -14 = Entry not found        *
 **									*
 * Log:									*
 * S. Gilbert/NCEP		 11/2004				*
 ***********************************************************************/
{

    char tmpname[LLMXLN];
    int n, ier;
    static char deftbl[]="wmocenter.tbl";
    static char currtable[LLMXLN];
    static G2wmocntrs cntrtbl={0,0};

/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Check if user supplied table.  If not, use default.
     */
    if ( strlen(tblnam) == (size_t)0 ) {
        strcpy( tmpname, deftbl );
    }
    else {
        strcpy( tmpname, tblnam );
    }

    /*
     *  Check if table has already been read in. 
     *  If different table, read new one in.
     */
    if ( strcmp( tmpname, currtable ) != 0 ) {
        if ( cntrtbl.info != 0 ) {
            free(cntrtbl.info);
            cntrtbl.info=0;
            cntrtbl.nlines=0;
        }
        printf(" Opening WMO Originating Center Table %s...\n",tmpname);
        ctb_g2rdcntr( tmpname, &cntrtbl, &ier );
        if ( ier != 0 ) {
            cntrtbl.nlines=0;
            *iret=-13;
            return;
        }
    }
    strcpy( currtable, tmpname );

    /*
     *  Search through table for id.
     */
    *iret=-14;
    n=0;
    while ( n < cntrtbl.nlines ) {
        if ( cntrid == cntrtbl.info[n].id ) {
            strcpy( wmocntr, cntrtbl.info[n].abbrev );
            *iret=0;
            break;
        }
        n++;
    }

}
