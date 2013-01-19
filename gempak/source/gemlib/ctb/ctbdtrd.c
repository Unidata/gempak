#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

Data_t		DtTable;
int		DtReadin=G_FALSE;

void ctb_dtrd ( int *iret )
/************************************************************************
 * ctb_dtrd								*
 *									*
 * This function loads the data.tbl table into its structure.		*
 *									*
 * ctb_dtrd ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 * *iret		int	Return code				*
 *				  -1 - Unable to open data table	*
 *				  -2 - No valid records in data table	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/98	Created					*
 * D.W.Plummer/NCEP	 4/98	Updated for cfltbnr calling sequence	*
 *				change					*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 8/99	Changed format of data.tbl		*
 * S. Law/GSC		11/99	Changed CAT_MISC -> CAT_MSC		*
 * J. Wu/GSC		06/01	use new ctb_dcatstoi(), ctb_dscatstoi() *
 * T. Lee/SAIC		 9/04	added time binning option		*
 * m.gamazaychikov/SAIC	12/04	added new flag ionoff			*
 * T. Piper/SAIC	03/06	increased buff length to 144 from 120	*
 * T. Piper/SAIC	03/06	check iret of cfl_trln!			*
 * m.gamazaychikov/SAIC	04/06	add domtmmtch to DtTable structure	*
 * T. Piper/SAIC	05/06	Removed ctb_pfread calls		*
 * F. J. Yen/NCEP	04/08	Added mnsbfr,mnaftr,& mstrct to DtTable.*
 ***********************************************************************/
{
int	ii, jj, numtmplts, ier, catnum, scatnum, ionoff, ival;
char 	buff[165], tcat[9], ccat[9], dhrs[22], cval[5];
int	aftr, bfr, tmmch, mnaftr, mnbfr, mstrct;
FILE	*fp;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    if ( DtReadin == G_TRUE )  return;

    /*
     *  Open the data table. If not found return an error.
     */

    fp = cfl_tbop( DATA_TBL, "config", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }

    /* 
     *  Get the number of templates.
     */

    cfl_tbnr( fp, &numtmplts, iret );

    if ( numtmplts != 0 )  {

        /* 
         *  Allocate the data strucure elements.
         */
        DtTable.numtmpl  = numtmplts; 
        DtTable.info     = (DTinfo *) malloc( numtmplts * sizeof(DTinfo) );
    }
    else  {
	cfl_clos(fp, &ier);
        *iret = -2;
        return;
    }

    rewind(fp);

    /* 
     *  Go through the file again and store each template.
     */

    ii = jj = 0;
    while ( ii < numtmplts && !feof(fp) ) {
    
	/*
	 *  Next template; process
	 */

        cfl_trln(fp, sizeof(buff), buff, iret);
	if ( *iret != 0 ) {
	    cfl_clos(fp, &ier);
	    return;
	}

	jj = sscanf( buff, "%s %s %s %s %s %d %d %d %s %d", 
			DtTable.info[ii].alias,
			DtTable.info[ii].path,
			DtTable.info[ii].template,
			ccat, tcat,
			&DtTable.info[ii].nframe,
			&DtTable.info[ii].range,
			&DtTable.info[ii].intrvl,
			dhrs,
			&tmmch);
	
	ctb_dcatstoi( ccat, &catnum, &ier ); 
	DtTable.info[ii].catgry = catnum;
	ctb_dscatstoi( tcat, &scatnum, &ier );
	DtTable.info[ii].subcat = scatnum;

	aftr = bfr = ionoff = mnbfr = mnaftr = mstrct = 0;
	if ( jj >= 9 ) {
	    ctb_dhrsstoi (dhrs, &ionoff, &bfr, &mnbfr, &aftr, &mnaftr,
		          &mstrct, &ier );
	}
	DtTable.info[ii].ionoff = ionoff;
	DtTable.info[ii].hrsbfr = bfr;
	DtTable.info[ii].mnsbfr = mnbfr;
	DtTable.info[ii].hraftr = aftr;
	DtTable.info[ii].mnaftr = mnaftr;
	DtTable.info[ii].mstrct = mstrct;

	if ( jj == 10 ) {
	    if ( tmmch >= 1 && tmmch <=4 ) {
                DtTable.info[ii].domtmmtch = tmmch;
            }
            else {
               ctb_pfstr ( "TIME_MATCH", cval, &ier );
               ival = atoi ( cval );
	       if ( ival >= 1 && ival <=4 && ier == 0 ) {
                   DtTable.info[ii].domtmmtch = ival;
	       }
               else {
                   DtTable.info[ii].domtmmtch = 4;
               }
            }
	}
        else {
           ctb_pfstr ( "TIME_MATCH", cval, &ier );
           ival = atoi ( cval );
	   if ( ival >= 1 && ival <=4 && ier == 0 ) {
               DtTable.info[ii].domtmmtch = ival;
           }
           else {
               DtTable.info[ii].domtmmtch = 4;
           }
	}

	ii++;

    }

    cfl_clos(fp, &ier);

    DtReadin = G_TRUE;

}
