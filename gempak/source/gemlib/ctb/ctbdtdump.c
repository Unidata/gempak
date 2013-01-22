#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern	Data_t		DtTable;
extern	int		DtReadin;

void ctb_dtdump ( int *iret )
/************************************************************************
 * ctb_dtdump								*
 *									*
 * This function dumps the contents of the DtTable structure.		*
 *									*
 * ctb_dtdump ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 * *iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 * I. Durham/GSC	 5/98   Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 9/98	Increased length of the path		*
 * S. Jacobs/NCEP	 8/99	Changed format of data.tbl		*
 * T. Lee/SAIC		 9/04	added bin hours				*
 * m.gamazaychikov/SAIC	12/04	added ionoff to the print statement	*
 * m.gamazaychikov/SAIC	01/06	Changed format spec in the print stmnt	*
 * T. Piper/SAIC	03/06	Check iret of ctb_dtrd!			*
 * m.gamazaychikov/SAIC	12/04	add domtmmtch to the print statement	*
 * F. J. Yen/NCEP	04/08	Added binning minutes & most recent flag*
 ***********************************************************************/
{
int	i;
char	tcat[][9] = { "SCAT_NIL", "SCAT_SFC", "SCAT_SHP",
		      "SCAT_SFF", "SCAT_FFG", "SCAT_SND",
		      "SCAT_SNF", "SCAT_FCT", "SCAT_ANL" };
char	ccat[][9] = { "CAT_NIL", "CAT_IMG", "CAT_SFC", "CAT_SFF",
		      "CAT_SND", "CAT_SNF", "CAT_GRD", "CAT_VGF",
		      "CAT_MSC" };

/*---------------------------------------------------------------------*/
    *iret = 0;

    if ( DtReadin == G_FALSE )  {
	ctb_dtrd ( iret );
    }

    if ( *iret == 0 ) {

        for ( i = 0; i < DtTable.numtmpl; i++ )  {

	    printf("%2d %-12s %-25s %-48s %-8s(%d) %-8s(%d) %5d %5d %5d %5s %5d:%02d %5d:%02d %5s %5d\n",
		i, 
		DtTable.info[i].alias, 
		DtTable.info[i].path,
		DtTable.info[i].template,
		ccat[DtTable.info[i].catgry],
		DtTable.info[i].catgry,
		tcat[DtTable.info[i].subcat],
		DtTable.info[i].subcat,
		DtTable.info[i].nframe,
		DtTable.info[i].range,
		DtTable.info[i].intrvl,
		(DtTable.info[i].ionoff == 1)?"ON":"OFF",
		DtTable.info[i].hrsbfr,
		DtTable.info[i].mnsbfr,
		DtTable.info[i].hraftr, 
		DtTable.info[i].mnaftr, 
		(DtTable.info[i].mstrct == 1)?"ON":"OFF",
		DtTable.info[i].domtmmtch );

        }
    }

    return;

}
