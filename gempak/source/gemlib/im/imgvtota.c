#include "geminc.h" 
#include "gemprm.h"
#include "imgdef.h"

#define IMGCOEFFS_TBL   "imgcoeffs.tbl"

typedef struct imgconv {
    char        sat_id[8];
    int         sat_num;
    int         chan;
    int         det;
    double      scal_m;
    double      scal_b;
    int         side;
    double      conv_n;
    double      conv_a;
    double      conv_b;
    double      conv_g;
} ImgCoeffs;

static  int     	readIMGCOEFFS=0;
static  int     	nImgRecs;
static	ImgCoeffs	*ImageConvInfo;
/*
 * Conversion coefficients
 */
static  double  	c1 = 1.191066E-5;
static  double  	c2 = 1.438833;


void im_gvtota ( int *nvals, unsigned int *gv, float *ta, int *iret )
/************************************************************************
 * im_gvtota								*
 *									*
 * This subroutine converts GVAR counts to actual temperatures 		*
 * based on the current image set in IM_SIMG.				*
 *                                                                      *
 * im_gvtota ( int *nvals, unsigned int *gv, float *ta, int *iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*nvals		int	Number of values to convert		*
 *	*gv		int	Array of GVAR count values		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*ta		float	Array of actual temperatures		*
 *	*iret 		int	Return value				*
 *				= -1 - could not open table		*
 *				= -2 - could not find match		*
 *                                                                      *
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	02/03						*
 * D.W.Plummer/NCEP	06/03	Add coeff G for 2nd order poly conv	*
* T. Piper/SAIC		07/06	Added tmpdbl to eliminate warning	*
 ***********************************************************************/
{
int		ii, ip, chan, found, bufsiz, ier;
char		buffer[128];
double		Rad, Teff, tmpdbl;
FILE		*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;
    for ( ii = 0; ii < *nvals; ii++ )  {
	ta[ii] = RMISSD;
    }
 
    /*
     *    Read in coefficient table if necessary.
     */
    if ( readIMGCOEFFS == 0 )  {

	fp = cfl_tbop ( IMGCOEFFS_TBL, "sat", &ier );

	if ( ier != 0 )  {
	    *iret = -1;
	    return;
	}
        else  {
	   
	    bufsiz = sizeof ( buffer );

            cfl_tbnr ( fp, &nImgRecs, &ier );

            ImageConvInfo = 
		(ImgCoeffs *)malloc((size_t)nImgRecs * sizeof(ImgCoeffs));

	    for ( ii = 0; ii < nImgRecs; ii++ )  {

		cfl_trln ( fp, bufsiz, buffer, &ier );

		sscanf ( buffer, "%s %d %d %d %lf %lf %d %lf %lf %lf %lf", 
		    ImageConvInfo[ii].sat_id, &ImageConvInfo[ii].sat_num,
		   &ImageConvInfo[ii].chan,   &ImageConvInfo[ii].det,
		   &ImageConvInfo[ii].scal_m, &ImageConvInfo[ii].scal_b,
		   &ImageConvInfo[ii].side,   &ImageConvInfo[ii].conv_n,
		   &ImageConvInfo[ii].conv_a, &ImageConvInfo[ii].conv_b, 
		   &ImageConvInfo[ii].conv_g );

	    }

	    readIMGCOEFFS = 1;
        }

    }

    found = 0;
    ii = 0;
    while ( ii < nImgRecs && found == 0 )  {

	tmpdbl = pow ( 2.0, (double)(ImageConvInfo[ii].chan-1));
	chan = G_NINT (tmpdbl);
	
	if ( ( imsorc == ImageConvInfo[ii].sat_num ) && 
	     ( imtype == chan ) )  {
	    found = 1;
	}
	else  {
	    ii++;
	}

    }

    if ( found == 0 )  {
	*iret = -2;
	return;
    }
    else  {

	ip = ii;
	for ( ii = 0; ii < *nvals; ii++ )  {

	  /*
	   *  Convert GVAR count (gv) to Scene Radiance
	   */
	  Rad = ( (double)gv[ii] - ImageConvInfo[ip].scal_b ) / 
/*              -------------------------------------		 	  */
		     ImageConvInfo[ip].scal_m ;

	  Rad = G_MAX ( Rad, 0.0 );

	  /*
	   *  Convert Scene Radiance to Effective Temperature
	   */
	  Teff =              ( c2 * ImageConvInfo[ip].conv_n ) /
/*               -------------------------------------------------------   */
	         (log(1.0 + (c1 * pow(ImageConvInfo[ip].conv_n,3.0)) / Rad)) ;

	  /*
	   *  Convert Effective Temperature to Temperature
	   */
	  ta[ii] = (float)(ImageConvInfo[ip].conv_a + ImageConvInfo[ip].conv_b * Teff +
	      		ImageConvInfo[ip].conv_g * Teff * Teff);

	}

    }

}
