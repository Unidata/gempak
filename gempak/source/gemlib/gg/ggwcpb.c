#include "geminc.h"
#include "gemprm.h"

void gg_wcpb ( int *ibun, char *dattim, char *typein, char *strtin, 
		char *endin, char *cnum, char *ltln, int *iret)
/************************************************************************
 * gg_wcpb								*
 *                                                                      *
 * This subroutine transforms four strings:watch type, start time, end  *
 * time, watch number and lat/lon points with each separated by '|' 	*
 * into character arrays. 						*
 *                                                                      *
 *  gg_wcpb ( ibun, dattim, typin, srtrin, endin, cnum, ltln, iret)	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ibun		int		Number of watches		*
 *	*dattim		char		Date/time - GEMPAK format	*
 *	*typein		char		UG codes string			*
 *	*strtin		char		State id string			*
 *	*endin		char		County name string		*
 *	*cnum		char		Date (DD)			*
 *	*ltln		char		Time (HHMM)			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 4/05						*
 ***********************************************************************/
{
     int     ii, ier, numstr, num, maxlt;
     int     maxch, len, ilen, lenc, lens;
     char    **typ_arr, **str_arr, **end_arr, **wnm_arr, **ltln_arr;
/*-------------------------------------------------------------------*/
     *iret  = 0;
     ier    = 0;
     len    = 4;
     lens   = DTTMSZ;
     lenc   = 5;
     maxch  = 32;
     maxlt  = 140;


    /*
     * Allocate memory for array size.
     */

    num = *ibun;
    if (  num != 0 ) {
        typ_arr = (char **)malloc(num * sizeof(char *));
        str_arr = (char **)malloc(num * sizeof(char *));
        end_arr = (char **)malloc(num * sizeof(char *));
        wnm_arr = (char **)malloc(num * sizeof(char *));
        ltln_arr = (char **)malloc(num * sizeof(char *));

        typ_arr[0] = '\0';
        str_arr[0] = '\0';
        end_arr[0] = '\0';
        wnm_arr[0] = '\0';
        ltln_arr[0] = '\0';

        for ( ii = 0; ii < num; ii++ ) {

	    typ_arr[ii] = (char *)malloc((len+1) * sizeof(char));
	    str_arr[ii] = (char *)malloc((lens+1) * sizeof(char));
	    end_arr[ii] = (char *)malloc((lens+1) * sizeof(char));
	    wnm_arr[ii] = (char *)malloc((lenc+1) * sizeof(char));
	    ltln_arr[ii] = (char *)malloc((maxlt+1) * sizeof(char));
        }

        /*
         * Break apart the UG code, county/zones and state id strings into 
         * arrays.
         */

         cst_lstr ( typein, &ilen, &ier );
         cst_rmbl ( typein, typein, &ilen, &ier);
         cst_clst ( typein, '|', " ", num, maxch, typ_arr, &numstr, &ier);

         cst_lstr ( strtin, &ilen, &ier );
         cst_rmbl ( strtin, strtin, &ilen, &ier);
         cst_clst ( strtin, '|', " ", num, maxch, str_arr, &numstr, &ier);

         cst_lstr ( endin, &ilen, &ier );
         cst_rmbl ( endin, endin, &ilen, &ier);
         cst_clst ( endin, '|', " ", num, maxch, end_arr, &numstr, &ier);

         cst_lstr ( cnum, &ilen, &ier );
         cst_rmbl ( cnum, cnum, &ilen, &ier);
         cst_clst ( cnum, '|', " ", num, maxch, wnm_arr, &numstr, &ier);

         cst_lstr ( ltln, &ilen, &ier );
         cst_rmbl ( ltln, ltln, &ilen, &ier);
         cst_clst ( ltln, '|', " ", num, maxlt, ltln_arr, &numstr, &ier);
     }

    /*
     * Get list of counties and zones in a UGC formatted string.
     */

     wbc_wcp ( ibun, dattim, typ_arr, str_arr, end_arr, wnm_arr,
       	       ltln_arr, &ier );

    /*
     * Free memory.
     */

    if ( num!= 0 ) {
        for( ii = 0; ii < num; ii++ ) {
            free( typ_arr[ii] );
	    free( str_arr[ii] );
	    free( end_arr[ii] );
	    free( wnm_arr[ii] );
	    free( ltln_arr[ii] );
        }

        if ( num > 0 ) { 
	    free( (char **) typ_arr );
	    free( (char **) str_arr );
	    free( (char **) end_arr );
	    free( (char **) wnm_arr );
    	    free( (char **) ltln_arr );
        }
    }
}
