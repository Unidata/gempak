#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

#define 	NCOLN		110 

G2vars_t	Gr2Tbl;
char		Gr2Readin = 0;

void ctb_g2read ( int *iret ) 
/************************************************************************
 * ctb_g2read								*
 *									*
 * This routine will read a grib2 table into an array of structures.	*
 *									*
 * ctb_g2read ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * m.gamazaychikov/SAIC	 5/03						*
 * m.gamazaychikov/SAIC	 5/03	added parameter pdtnmbr to the table	*
 * M. Li/SAIC		 4/04	added hzremap, and direction		*
 ***********************************************************************/
{
FILE     *fp;
int      n, nr, blen, ier;
char     buffer[256]; 
int      disc, cat, parm, pdtn, scl, ihzrmp, idrct;
char     name[33], unts[21], gname[13];
float    msng;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

        if ( Gr2Readin == 1 )  return;

        /*
         *  Open the table. If not found return an error.
         */

        fp = cfl_tbop( G2VARS_TBL, "grid", &ier);
        if ( fp == NULL  ||  ier != 0 )  {
            *iret = -1;
            return;
        }

        cfl_tbnr(fp, &nr, &ier);
        if ( ier != 0 || nr == 0 ) {
            *iret = -2;
            cfl_clos(fp, &ier);
            return;
        }

        Gr2Tbl.info = (G2Vinfo *)malloc((size_t)nr*sizeof(G2Vinfo));

        n  = 0;
        while ( n < nr ) {

            cfl_trln( fp, 256, buffer, &ier );
            if ( ier != 0 ) break;

	    cst_lstr (  buffer, &blen, &ier );

	    if ( blen > NCOLN ) { 
                sscanf( buffer, "%d %d %d %d %32c %20c %s %d %f %d %d", 
                            &disc, &cat, &parm, &pdtn,
                            name, unts, gname,
                            &scl, &msng, &ihzrmp, &idrct );
	    }
	    else {
		sscanf( buffer, "%d %d %d %d %32c %20c %s %d %f",
                            &disc, &cat, &parm, &pdtn,
                            name, unts, gname,
                            &scl, &msng);
		ihzrmp = 0;
		idrct = 0;
	    }

	    name[32] = '\0';
	    unts[20] = '\0';

            Gr2Tbl.info[n].discpln=disc;
            Gr2Tbl.info[n].categry=cat;
            Gr2Tbl.info[n].paramtr=parm;
            Gr2Tbl.info[n].pdtnmbr=pdtn;
            strcpy(Gr2Tbl.info[n].name,    name);
            strcpy(Gr2Tbl.info[n].units,   unts);
            strcpy(Gr2Tbl.info[n].gemname, gname);

            Gr2Tbl.info[n].scale=scl;
            Gr2Tbl.info[n].missing=msng;
	    Gr2Tbl.info[n].hzremap = ihzrmp;
	    Gr2Tbl.info[n].direction = idrct;

            n++;
        }


        cfl_clos(fp, &ier);

        Gr2Tbl.nlines = n;

        Gr2Readin = 1;

}
