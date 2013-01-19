#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"


void ctb_g2rdvar ( char *tbname, G2vars_t *vartbl, int *iret ) 
/************************************************************************
 * ctb_g2rdvar								*
 *									*
 * This routine will read a GRIB2 Parameter                             *
 * table into an array of structures.	                                *
 * The table is allocated locally and a pointer to the new table is     *
 * passed back to the user in argument vartbl.  The user is resposible  *
 * for freeing this memory, when the table is no longer needed, by      *
 * free(vartbl.info)                                                    *
 *									*
 * ctb_g2rdvar ( tbname, vartbl, iret )				        *
 *									*
 * Input parameters:							*
 *	*tbname		char		Filename of the table to read   *
 *									*
 * Output parameters:							*
 *	*vartbl	G2vars_t	Pointer to list of table entries        *
 *	*iret		int		Return code			*
 *                                        0 = Successful                *
 *                                       -1 = Could not open            *
 *                                       -2 = Could not get count of    *
 *                                            of table entries.         *
 **									*
 * Log:									*
 * S. Gilbert/NCEP	 11/04	Modified from ctb_g2rdcntr to read a    *
 *                              GRIB2 Parameter Table.                  *
 ***********************************************************************/
{
        FILE     *fp;
        int      n, blen,  nr, ier;
        char     buffer[256]; 
        char     name[33], gname[13], unts[21];
        int      disc, cat, parm, pdtn, scl, ihzrmp, idrct;
        float    msng;

        const    int  ncoln=110;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

        /*
         *  Open the table. If not found return an error.
         */

        fp = cfl_tbop( tbname, "grid", &ier);
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

        vartbl->info = (G2Vinfo *)malloc((size_t)nr*sizeof(G2Vinfo));
        vartbl->nlines = nr;

        n  = 0;
        while ( n < nr ) {

            cfl_trln( fp, 256, buffer, &ier );
            if ( ier != 0 ) break;

	    cst_lstr (  buffer, &blen, &ier );

            if ( blen > ncoln ) {
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
	    gname[12] = '\0';

            vartbl->info[n].discpln=disc;
            vartbl->info[n].categry=cat;
            vartbl->info[n].paramtr=parm;
            vartbl->info[n].pdtnmbr=pdtn;
            strcpy(vartbl->info[n].name,    name);
            strcpy(vartbl->info[n].units,   unts);
            strcpy(vartbl->info[n].gemname, gname);
            vartbl->info[n].scale=scl;
            vartbl->info[n].missing=msng;
            vartbl->info[n].hzremap = ihzrmp;
            vartbl->info[n].direction = idrct;

            n++;
        }

        cfl_clos(fp, &ier);

}
