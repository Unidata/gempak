#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern G2vars_t         Gr2Tbl;
extern char             Gr2Readin;

void ctb_g2gnam ( int *discpln, int *categry, int *number, int *pdnumber,
		  char *g2name, int *ihzrmp, int *idrct, int *iret )
/************************************************************************
 * ctb_g2gnam								*
 *									*
 * Given a set of parameters (displine, category, number) 		*
 * this routine will return a GEMPAK name from the grib2 table.		*
 *									*
 * ctb_g2gnam ( discpln, categry, number, pdnumber, g2name, ihzrmp,	*
 *  		idrct, iret )						*
 *									*
 * Input parameters:							*
 *	*discpln	int	table discipline number			*
 *	*categry	int	table category				*
 *	*number		int	table number				*
 *      *pdnumber	int     product definition template number	*
 *									*
 * Output parameters:							*
 *	*g2name		char	GEMPAK name of the variable		*
 *	*hzremap	int	horizontal remaping			*
 *	*direction	int	direction				*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * m.gamazaychikov/SAIC	 5/03						*
 * m.gamazaychikov/SAIC	 5/03	added pdnumber as an input parameter	*
 * M. Li/SAIC		 4/04	added new parameters ihzrmp, idrct	*
 ***********************************************************************/
{
	int	i, ndline, dis, cat, num, pdt;
        char    *nfound = "\0";

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;
        if ( Gr2Readin == 0 )  {
                *iret = -1;
                return;
        }

        ndline = Gr2Tbl.nlines;
/*
 *	look thru the table
 */
        for ( i = 0; i < ndline; i++ ) {
               dis = Gr2Tbl.info[i].discpln;
               cat = Gr2Tbl.info[i].categry;
               num = Gr2Tbl.info[i].paramtr;
               pdt = Gr2Tbl.info[i].pdtnmbr;
               if ( ( *discpln  == dis ) &&
                    ( *categry  == cat ) &&
                    ( *number   == num ) &&
                    ( *pdnumber == pdt ) ) {
                    strcpy ( g2name, Gr2Tbl.info[i].gemname);

		    *ihzrmp = Gr2Tbl.info[i].hzremap;
		    *idrct = Gr2Tbl.info[i].direction;
                    return;
               }
        }

         strcpy ( g2name, nfound);
	 *ihzrmp = 0;
	 *idrct = 0;
         *iret = -4;
}
