#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_cmbn ( shp_record *shprec, int *numrec, int *iret )
/************************************************************************
 * shp_cmbn                                                             *
 *                                                                      *
 * This function combines those records with the same key into		*
 * one record.								*
 *                                                                      *
 * shp_cmbn ( shprec, numrec, iret )                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *shprec         shp_record  	Shape record list		*
 *	*numrec		int		Number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 ***********************************************************************/
{
    shp_record *currec, *cmprec, *tmprec;
    shp_part **curprtp;
    char curkey[MAXRECLEN], cmpkey[MAXRECLEN];
    int nrec, irec, jrec, kprt, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    nrec = *numrec;
    for ( currec = shprec, irec = 0; irec < nrec; 
        irec++, currec = currec->nxtrec ) {

	shp_gkey ( currec, curkey, &ier );

        for ( cmprec = currec->nxtrec, jrec = irec + 1; 
	    jrec < nrec; jrec++, cmprec = cmprec->nxtrec ) {

	    shp_gkey ( cmprec, cmpkey, &ier );

	    if ( strcmp ( curkey, cmpkey ) == 0 ) {
	        /*
		 * These two records have the same key, combine them.
		 */
	        for ( curprtp = &(currec->shpart), kprt = 0; 
		    kprt < currec->numprt;
		    kprt++, curprtp = &((*curprtp)->nxtprt) ) {
		    /*
		     * Go through all of the parts of the current record.
		     */
		}
		*curprtp = cmprec->shpart;
		currec->numprt += cmprec->numprt;

		/*
		 * Remove cmprec from record list.
		 */
		if ( cmprec->prvrec != NULL ) {
		    cmprec->prvrec->nxtrec = cmprec->nxtrec;
		}
		if ( cmprec->nxtrec != NULL ) {
		    cmprec->nxtrec->prvrec = cmprec->prvrec;
		}
		tmprec = cmprec;
		cmprec = cmprec->prvrec;
		jrec -= 1;
		shp_mfree ( tmprec );
		nrec -= 1;
	    }
	}
    }
    *numrec = nrec;
}
