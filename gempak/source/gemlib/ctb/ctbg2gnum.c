#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

extern G2vars_t         Gr2Tbl;
extern char             Gr2Readin;

void ctb_g2gnum ( char *g2name, 
                  int *discpln, int *categry, int *number, int *pdnumber,
                  int *iret )
/************************************************************************
 * ctb_g2gnum								*
 *									*
 * Given a GEMPAK name from the grib2 table this routine will return a	*
 * set of parameters (displine, category, number) 			*
 *									*
 * ctb_g2gnum ( g2name, discpln, categry, number, pdtnmbr, iret )	*
 *									*
 * Input parameters:							*
 *	*g2name		char	GEMPAK name of the variable		*
 *									*
 * Output parameters:							*
 *	*discpln	int	table discipline number			*
 *	*categry	int	table category				*
 *	*number		int	table number				*
 *      *pdtnmbr        int     product definition template number      *
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * m.gamazaychikov/SAIC	  5/03						*
 * m.gamazaychikov/SAIC	  5/03	added pdtnmbr as an output parameter;	*
 *				if a name has two consequtive numbers 	*
 *				substitute the numbers with dashes	*
 ***********************************************************************/
{
	int	jj, i, ndline, iret1;
        char    lg2name[13];
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;
        if ( Gr2Readin == 0 )  {
                *iret = -1;
                *discpln  = IMISSD;
                *categry  = IMISSD;
                *number   = IMISSD;
                *pdnumber = IMISSD;
                return;
        }

        ndline = Gr2Tbl.nlines;

        cst_lcuc (g2name, lg2name, &iret1);

/*
 * if a name has two consequtive numbers substitute the numbers
 * with dashes
 */
        for (jj = 0; jj < 3;jj++) {
            if( isdigit( (unsigned long)lg2name[jj] ) ) {
              if( isdigit( (unsigned long)lg2name[jj+1] ) ) {
                  lg2name[jj]   = '-';
                  lg2name[jj+1] = '-';
              }
            }
        }
/*
 *	look thru the table
 */
        for ( i = 0; i < ndline; i++ ) {
            if (strcmp(lg2name,Gr2Tbl.info[i].gemname ) == 0) {
                  *discpln = Gr2Tbl.info[i].discpln;
                  *categry = Gr2Tbl.info[i].categry;
                  *number  = Gr2Tbl.info[i].paramtr;
                  *pdnumber = Gr2Tbl.info[i].pdtnmbr;
                  return;
             }
        }
        *discpln  = IMISSD;
        *categry  = IMISSD;
        *number   = IMISSD;
        *pdnumber = IMISSD;
        *iret = -5;
}
