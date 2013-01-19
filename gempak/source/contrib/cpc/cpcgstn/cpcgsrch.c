#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"

#define	BNDTYP		"US_AK"
#define	MXBND		10
#define STNFILDIR       "stns"

void cpcg_srch ( char *tblnam, char *fname, int *nstn, 
			int *istnm, int *iflag, int *iclr, int *iret )
/************************************************************************
 * cpcg_srch                                              		*
 *                                                                      *
 * This function searches which stations are located inside the         *
 * given polygon. If the polygon is open, construct a closed polygon	*
 * using the open polygon and the US bounds.				*  
 *                                                                      *
 * cpcg_srch (tblnam, fname, nstn, istnm, iflag, iclr, iret)		* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*tblnam		char	Station table file name			*
 *	*fname		char	Polygon vg file name			*
 *									*
 * Output parameters:                                                   *
 *	*nstn		int	Number of station in the table		*
 *	*istnm		int	Station number				*
 *	*iflag		int	Array of in or out results		*
 *				1 = station inside polygon		*
 *				0 = station outside polygon		*
 *	*iclr		int	Color of polygon			*
 *      *iret    int    Return code                                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		08/01   Created                                 *
 ***********************************************************************/
{
int  	ii, jj, kk, nbnd, maxstn, np3, ier;
int     ispri[LLSTFL], npts[MAXPTS], lnpts[MAXPTS];
int     nlin, lcolr[MXBND], close[MXBND], inout[LLSTFL];

float   slat[LLSTFL], slon[LLSTFL], selv[LLSTFL];
float   blat[MXBND][LLMXPT], blon[MXBND][LLMXPT];
float   llat[MXBND][MAXPTS], llon[MXBND][MAXPTS];
float   xp3[MAXPTS], yp3[MAXPTS];

char    stid[LLSTFL][9], stnnam[LLSTFL][33], stat[LLSTFL][3],
        coun[LLSTFL][3], tbchrs[LLSTFL][21];

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Read the station table
     */
    maxstn = LLSTFL;
    ctb_astn ( tblnam, STNFILDIR, &maxstn, nstn, stid, stnnam,
                        istnm, stat, coun, slat, slon, selv,
                        ispri, tbchrs, &ier );

    if ( ier != 0 || *nstn <= 0 ) {
        *iret = -3;
        return;
    }

    for (ii = 0; ii < *nstn; ii++) {
	iflag[ii] = 0;
	iclr[ii] = 0;
    }

    /*
     *  Read bounds US bounds
     */
    cpcg_rdbnd(BNDTYP, &nbnd, npts, blat, blon, &ier);
    if ( ier != 0 || nbnd <= 0 ) {
	*iret = -4;
        return;
    }    

    /*
     *  Read vg files
     */
    cpcg_rdln(fname, &nlin, lnpts, lcolr, close, llat, llon, &ier);

    if ( ier != 0 ) {
        *iret = -2;
        return;
    } 

    /*
     * Check if the stations are inside the polygons
     */
    for (ii = 0; ii < nlin; ii++) {
	/*
	 * Close polygon
	 */
  	if ( close[ii] ) {
	    np3 = lnpts[ii];
	    for (kk = 0; kk < np3; kk++) {
		xp3[kk] = llat[ii][kk];
		yp3[kk] = llon[ii][kk];
	    } 
	}

	/*
	 * For open polygon, construct a closed polygon with the US bounds
         */ 
	else {
	   for (jj = 0; jj < nbnd; jj++) {
	       cpcg_newpoly(&lnpts[ii], llat[ii], llon[ii], &npts[jj], blat[jj], blon[jj],
			   &np3, xp3, yp3, &ier); 

	       if (ier == 0) break;
	   }
	}

	cgr_inpoly (sys_M, nstn, slat, slon, sys_M,
                        &np3, xp3, yp3, inout, &ier);

   	for (kk = 0; kk < *nstn; kk++) {
	    if ( iflag[kk] == 0 && inout[kk] == 1 ) {
	        iflag[kk] = 1;
	        iclr[kk] = lcolr[ii];
	    }
	}

    }

}
