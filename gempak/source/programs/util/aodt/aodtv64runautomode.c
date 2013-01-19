#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"
#include "AODT/v64/odtapi.h"

/* prototype */
void aodtv64_runautomode ( int fcsttype, char *fcstfile, char *imagefile,
                      float *cenlat, float *cenlon, int *posm );

void aodtv64_runautomode ( int fcsttype, char *fcstfile, char *imagefile, 
		      float *cenlat, float *cenlon, int *posm )
 /***********************************************************************
 * aodtv64_runautomode                                                  *
 *                                                                      *
 * This function will get an initial storm center from a given forecast *
 * file, and then use the given IR image to calculate the storm 	*
 * center position.							*
 *                                                                      *
 * void aodtv64_runautomode ( fcsttype, fcstfile, imagefile, cenlat, 	*
 *			      cenlon, posm )                            *
 *                                                                      *
 * Input  parameters:                                                   *
 *	fcsttype	int	Forecast file type			*
 *	*fcstfile	char	Forecast file				*
 *	*imagefile	char	Image file				*	
 * Output parameters:                                                   *
 *	*cenlat		float   latitude of storm center		*
 *	*cenlon		float   longitude of storm center		*
 *	*posm		int	positioning method			*	
 *                                                                      *
 * Log:                                                                 *
 * M. Li/SAIC           01/07	Created					* 
 * S. Gilbert/NCEP      01/07   Removed recalculation of lat/lons       *
 ***********************************************************************/
{
    int         indx;
    int         iaodt, ier, ier2, pos, tpos;
    int         radius, irad, ii, jj, numx, numy;
    size_t	ifl, ipr;
    float       flat, flon, wlat, wlon;
    float       *ftmps, *flats, *flons, cenlon2, tlat, tlon;
    float       **temps, **lats, **lons;
    char	atcftype[80];

/*---------------------------------------------------------------------*/
    	/*
     	 * Set forecast file in AODT       
     	 */
    	 strcpy(atcftype, "OFCL");
    	 iaodt=aodtv64_setforecastfile(fcstfile,fcsttype,atcftype);  

    	/*
     	 * determine initial auto-center position estimate
	 */
    	 iaodt=aodtv64_runautomode1(&flat,&flon,&pos);

    	/*
     	 *   Set center location in AODT
     	 */
    	 iaodt=aodtv64_setlocation(flat,flon,pos);


    	/*
     	 *   Read in IR image from imagefile 
     	 */
	 radius = 500;
    	 cenlon2 = -1.0 * flon;
         irad = radius/4 + 5;
         numx = numy = irad*2 + 1;
         ftmps = (float *)malloc( (size_t)((numx)*(numy))* sizeof(float) );
         flats = (float *)malloc( (size_t)((numx)*(numy)) * sizeof(float) );
         flons = (float *)malloc( (size_t)((numx)*(numy)) * sizeof(float) );
         im_gtmp ( imagefile, "dset", sys_M, &flat, &cenlon2, &irad,
                   &numx, &numy, ftmps, flats, flons, &ier2,
                   strlen(imagefile), strlen("dset"), strlen(sys_M) );

         if ( ier2 != 0 )  {
            er_wmsg  ( "IM", &ier2, " ", &ier, strlen("IM"), strlen(" ") );
            exit (0);
         }

         ifl = sizeof(float);
	 ipr = sizeof(float*);
         temps = (float **)calloc((size_t)numy, ipr);
         lats  = (float **)calloc((size_t)numy, ipr);
         lons  = (float **)calloc((size_t)numy, ipr);
         for ( jj = 0; jj < numy; jj++ ) {
            temps[jj] = (float *)calloc((size_t)numx, ifl);
            lats[jj]  = (float *)calloc((size_t)numx, ifl);
            lons[jj]  = (float *)calloc((size_t)numx, ifl);
         }
	
         for ( jj = 0; jj < (numy); jj++ )  {
             for ( ii = 0; ii < (numx); ii++ )  {
                 indx = jj*(numy)+ii;
                 temps[jj][ii] = ftmps[indx];
                 lats[jj][ii] = flats[indx];
                 lons[jj][ii] = flons[indx] *= -1.0F;
             }
         }
    
	 free ( flons );
         free ( flats );
         free ( ftmps );

	/*
	 *   Load the IR image information in AODT 
	 */
	 iaodt = aodtv64_loadIRimage ( temps, lats, lons, numx, numy );
	 for ( jj = 0; jj < numy; jj++ ) {
             free ( temps[jj] );
             free ( lats[jj]  );
             free ( lons[jj]  );
         }
         free( lons  );
         free( lats  );
         free( temps );

	/*
	 *   Set warmest pixel temp value in AODT  
	 */
 	 iaodt=aodtv64_getwarmeyetemplocation(&wlat, &wlon);
	
	/*
	 * determine "best" automated position
	 */
	 iaodt=aodtv64_runautomode2(flat, flon, &tlat, &tlon, &tpos);
	 *cenlat = tlat;
	 *cenlon = tlon;
	 *posm   = tpos; 
}

