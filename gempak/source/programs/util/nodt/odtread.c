#include "geminc.h"
#include "gemprm.h"
#include "inc/odt.h"

int readsatdata(char *,float,float,int,float [maxd][maxd],float [maxd][maxd],float [maxd][maxd],int *,int *);

int readsatdata(char *imagefile,float cenlat,float cenlon,int radius, 
	float temps[maxd][maxd],float lats[maxd][maxd],float lons[maxd][maxd], 
	int *numx,int *numy)
    /* 
     * Obtains a rectangular array of data from an N-AWIPS satellite image file.
     * Inputs : imagefile - image file name
     * cenlat     - center latitude location of data array
     * cenlon     - center longitude location of data array
     * radius     - outer radius of data read (km)
     * Output : temps      - array containing temperature values
     * lats       - array containing latitude values
     * lons       - array containing longitude values
     * numx       - number of elements in arrays
     * numy       - number of lines in arrays
     * Remarks:
     * This routine does not initialize the output array.
     * It does not reduce resolution.
     */

{
    int		ii, jj, indx, irad, ier, iret;
    int		one=1;
    float	dx, dy, clat, clon;
    float	*ftmps, *flats, *flons;
/*---------------------------------------------------------------------*/

    clat =  cenlat;
    /*
     * ODT code will pass longitude as west positive; make it west negative.
     */
    clon = -cenlon;

    /*
     * Convert center (lat,lon) into device coordinates.
     */
    gtrans ( sys_M, sys_D, &one, &clat, &clon, &dx, &dy, &ier,
	            strlen(sys_M), strlen(sys_D) );
    dx = (float)G_NINT ( dx );
    dy = (float)G_NINT ( dy );

    /*
     * Allocate memory for the data arrays.
     */
    irad = radius/4 + 5;
    *numx = *numy = irad*2 + 1;
    ftmps = (float *)malloc( (size_t)((*numx)*(*numy)) * sizeof(float) );
    flats = (float *)malloc( (size_t)((*numx)*(*numy)) * sizeof(float) );
    flons = (float *)malloc( (size_t)((*numx)*(*numy)) * sizeof(float) );

    /*
     * Retrieve temperatures from image.
     */
    im_gtmp ( imagefile, "dset", sys_D, &dx, &dy, &irad, 
	    numx, numy, ftmps, flats, flons, &iret,  
	    strlen(imagefile), strlen("dset"), strlen(sys_D) );
    if ( ier != 0 )  {
	er_wmsg  ( "IM", &iret, " ", &ier, strlen("IM"), strlen(" ") );
	exit (0);
    }

    /*
     *  Move local one-dim arrays into two-dim arrays.
     */
    for ( jj = 0; jj < (*numy); jj++ )  {
	for ( ii = 0; ii < (*numx); ii++ )  {
	    indx = jj*(*numy)+ii;
	    temps[jj][ii] = ftmps[indx];
	    lats [jj][ii] = flats[indx];
	    lons [jj][ii] = flons[indx] *= -1.0F;
	}
    }

    /*
     *  This doesn't make sense, but it parallels the original ODT code.
     */
    (*numx) -= 1;
    (*numy) -= 1;

    /*
     *  Free up all malloc'd memory.
     */
    free ( flons );
    free ( flats );
    free ( ftmps );

    return ( 0 );

}
