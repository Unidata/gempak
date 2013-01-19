#include "gb2def.h"

void gb2_navgdt( float *navblk, int *igdtnum, int *igdtmpl, int *iret)
/************************************************************************
 * gb2_navgdt								*
 *									*
 * This function converts a Gempak grid navigation block to an		* 
 * appropriate GRIB2 Grid Definition Template (GDT).                    *
 *									*
 * gb2_navgdt ( navblk, igdtnum, igdtmpl, iret )			*
 *									*
 * Input parameters:							*
 *      *navblk         float           Grid navigation block           *
 *									*
 * Output parameters:							*
 *      *igdtnum      	int             GRIB2 GDT number = N            *
 *      *igdtmpl      	int             GRIB2 GDT values for GDT 3.N    *
 *	*iret		int		Return code			*
 *					-35 = Unrecognized Proj         *
 *									*
 **									*
 * Log:									*
 * S. Gilbert/NCEP		 8/05					*
 ***********************************************************************/
{

        int     ier;
        int     kkx, kky;
        char    proj[4];
/*---------------------------------------------------------------------*/
	*iret = 0;

        gr_rnav ( navblk, proj, &kkx, &kky, &ier, 4);

	if ( strncmp(proj,"CED",3) == 0 ) { 
            *igdtnum = 0;     /* lat-lon   */
            gb2_gdtltln( navblk, igdtmpl, &ier );
        }
	else if ( strncmp(proj,"MER",3) == 0 ) { 
            *igdtnum = 10;    /* Mercator   */
            gb2_gdtmerc( navblk, igdtmpl, &ier );
        }
	else if ( strncmp(proj,"STR",3) == 0 ) { 
            *igdtnum = 20;    /* Polar Stereographic */
            gb2_gdtpstr( navblk, igdtmpl, &ier );
        }
	else if ( strncmp(proj,"LCC",3) == 0 ) { 
            *igdtnum = 30;    /* Lambert Conformal */
            gb2_gdtlamb( navblk, igdtmpl, &ier );
        }
        else { 
            *iret=-35;
            er_wmsg("GB", iret, proj, &ier, 2, 4 );
        }

}
