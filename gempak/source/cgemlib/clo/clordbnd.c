#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

void clo_rdbnd ( Bnd_t *bnd, char *fnm, int *iret )
/************************************************************************
 * clo_rdbnd								*
 *									*
 * This function reads a bounds info file into the CLO structures.	*
 *									*
 * clo_rdbnd ( bnd, fnm, iret )						*
 *									*
 * Input parameters:							*
 * *bnd		Bnd_t	Boundary structure				*
 * *fnm		char	File name					*
 *									*
 * Output parameters:							*
 * *iret	int	Return code					*
 *			= -1 - Unable to open bnds info table		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/00	Created					*
 ***********************************************************************/
{
int	ii, jj, len, ier;
char 	buff[512];

char	tstr[512];

FILE	*fp;
/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    /*
     *  Open the bounds info table. If not found return an error.
     */

    fp = (FILE *)cfl_tbop(fnm, "bounds", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
	bnd->nbnd = 0;
        *iret = -1;
        return;
    }

    cfl_trln(fp, sizeof(buff), buff, &ier);
    sscanf ( buff, "%s", tstr );
    bnd->filename = (char *)malloc( sizeof(char) * strlen(tstr)+1 );
    strcpy( bnd->filename, tstr );

    cfl_trln(fp, sizeof(buff), buff, &ier);
    sscanf ( buff, "%d", &(bnd->nbnd) );

    cfl_trln(fp, sizeof(buff), buff, &ier);
    sscanf ( buff, "%d", &(bnd->maxpts) );

    bnd->bound = (BInfo_t *)malloc( (size_t)bnd->nbnd * sizeof(BInfo_t) );  

    /* 
     *  Loop over bounds
     */
    for ( ii = 0; ii < bnd->nbnd; ii++ )  {

        cfl_trln(fp, sizeof(buff), buff, &ier);

        sscanf ( buff, "%s %ld %f %f %f %f %f %f %d",
		    tstr,
		    &(bnd->bound[ii].strec),
		    &(bnd->bound[ii].cenlat),
		    &(bnd->bound[ii].cenlon),
		    &(bnd->bound[ii].minlat),
		    &(bnd->bound[ii].minlon),
		    &(bnd->bound[ii].maxlat),
		    &(bnd->bound[ii].maxlon),
		    &(bnd->bound[ii].nparts) );

	bnd->bound[ii].name = 
	    (char *)malloc( sizeof(char) * strlen(tstr)+1 );
	strcpy( bnd->bound[ii].name, tstr );
	cst_lcuc ( bnd->bound[ii].name, bnd->bound[ii].name, &ier );

	bnd->bound[ii].bndspt = 
	    (Bndsprt_t *)malloc( (size_t)bnd->bound[ii].nparts * sizeof(Bndsprt_t) );

        cfl_trln(fp, sizeof(buff), buff, &ier);

	cst_rmbl( buff, buff, &len, &ier );
	bnd->bound[ii].info = 
	    (char *)malloc( sizeof(char) * strlen(buff)+1 );
	strcpy( bnd->bound[ii].info, buff );

	for ( jj = 0; jj < bnd->bound[ii].nparts; jj++ )  {

	    cfl_trln(fp, sizeof(buff), buff, &ier);

            sscanf ( buff, "%ld %f %f %f %f %d",
			&(bnd->bound[ii].bndspt[jj].strec),
                        &(bnd->bound[ii].bndspt[jj].minlat),
                        &(bnd->bound[ii].bndspt[jj].minlon),
                        &(bnd->bound[ii].bndspt[jj].maxlat),
                        &(bnd->bound[ii].bndspt[jj].maxlon),
                        &(bnd->bound[ii].bndspt[jj].npts) );

	}

    }

    cfl_clos(fp, &ier);

/*
 *  Diagnostic print
 */
/*
    printf("Total number of bounds in file %s = %d\n", 
           bnd->filename, bnd->nbnd );
    printf("Maximum number of points per bound = %d\n", bnd->maxpts );

        for ( ii = 0; ii < bnd->nbnd; ii++ )  {

	    printf("\t%-s %-12ld %-.2f %-.2f %-.2f %-.2f %-.2f %-.2f %-3d\n",
		    bnd->bound[ii].name, 
		    bnd->bound[ii].strec, 
		    bnd->bound[ii].cenlat, 
		    bnd->bound[ii].cenlon, 
		    bnd->bound[ii].minlat, 
		    bnd->bound[ii].minlon, 
		    bnd->bound[ii].maxlat, 
		    bnd->bound[ii].maxlon,
		    bnd->bound[ii].nparts );

            for ( kk = 0; kk < bnd->bound[ii].nparts; kk++ )  {

	        printf("\t\t%-12ld %-.2f %-.2f %-.2f %-.2f %-8d \n",
			bnd->bound[ii].bndspt[kk].strec, 
			bnd->bound[ii].bndspt[kk].minlat, 
			bnd->bound[ii].bndspt[kk].minlon, 
			bnd->bound[ii].bndspt[kk].maxlat, 
			bnd->bound[ii].bndspt[kk].maxlon, 
			bnd->bound[ii].bndspt[kk].npts );

            }
        }
*/

}
