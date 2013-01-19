#include "gb2def.h"

void gb2_diag ( gribfield *gfld, int gdflag )
/************************************************************************
 * gb2_diag								*
 *									*
 * This function prints out contents of specific GRIB2 sections based   *
 * the flags set in variable gdflag.                                    *
 *									*
 * gb2_diag ( gfld, gdflag )						*
 *									*
 * Input parameters:							*
 *	*gfld		gribmsg		decoded GRIB2 structure       	*
 *									*
 * Output parameters:							*
 *	gdflag		int		Diagnostic flag indicating      *
 *                                      which sections should be output *
 **									*
 * Log:									*
 * S.Gilbert/NCEP	12/04	New					*
 ***********************************************************************/
{

    int i;

/*---------------------------------------------------------------------*/

    printf("\n");
    /*
     *   Print out Section 0, if requested
     */
    if ( (gdflag & G2_IS) == G2_IS )  {
        printf ( " SECTION 0 - INDICATOR SECTION\n");
        printf ( "  IS byte        7 (discipline) = %d\n", (int)gfld->discipline );
        printf ( "  IS byte        8 (edition)    = %d\n\n", (int)gfld->version );
    } 

    /*
     *   Print out Section 1, if requested
     */
    if ( (gdflag & G2_IDS) == G2_IDS )  {
        printf ( " SECTION 1 - IDENTIFICATION SECTION\n");
        printf ( "  IDS bytes  6 -  7 (orig center)      = %d\n", (int)gfld->idsect[0] );
        printf ( "  IDS bytes  8 -  9 (sub center)       = %d\n", (int)gfld->idsect[1] );
        printf ( "  IDS byte       10 (Master table ver) = %d\n", (int)gfld->idsect[2] );
        printf ( "  IDS byte       11 (Local table ver)  = %d\n", (int)gfld->idsect[3] );
        printf ( "  IDS byte       12 (ref time sig.)    = %d\n", (int)gfld->idsect[4] );
        printf ( "  IDS bytes 13 - 14 (Year)             = %d\n", (int)gfld->idsect[5] );
        printf ( "  IDS byte       15 (Month)            = %d\n", (int)gfld->idsect[6] );
        printf ( "  IDS byte       16 (Day)              = %d\n", (int)gfld->idsect[7] );
        printf ( "  IDS byte       17 (Hour)             = %d\n", (int)gfld->idsect[8] );
        printf ( "  IDS byte       18 (Minute)           = %d\n", (int)gfld->idsect[9] );
        printf ( "  IDS byte       19 (Second)           = %d\n", (int)gfld->idsect[10] );
        printf ( "  IDS byte       20 (Prod status)      = %d\n", (int)gfld->idsect[11] );
        printf ( "  IDS byte       21 (type of data)     = %d\n\n", (int)gfld->idsect[12] );
    } 

    /*
     *   Print out Section 3, if requested
     */
    if ( (gdflag & G2_GDS) == G2_GDS )  {
        printf ( " SECTION 3 - GRID DEFINITION SECTION\n");
        printf ( "  GDS byte        6 (src of grid def) = %d\n", (int)gfld->griddef );
        printf ( "  GDS bytes  7 - 10 (no. of grid pts) = %d\n", (int)gfld->ngrdpts );
        printf ( "  GDS byte       11 (nb per opt list) = %d\n", (int)gfld->numoct_opt );
        printf ( "  GDS byte       12 (interp opt list) = %d\n", (int)gfld->interp_opt );
        printf ( "  Grid Definition Template 3.%d: ",(int)gfld->igdtnum);
        for (i = 0; i < gfld->igdtlen; i++ )
            printf ( " %d",(int)gfld->igdtmpl[i]);
        printf ( "\n" );

	if ( gfld->numoct_opt != 0 ) {
	   printf ( " OptionalOctets\n");
	   for ( i=0; i < gfld->num_opt; i++ )
		printf("   %d %d\n",i,gfld->list_opt[i]);
	}

        switch (gfld->igdtnum) {
         case 0:              /* lat-lon   */
         case 1:
         case 2:
         case 3:
            printf ( "  GDS bytes 31 - 34 (Nx)            = %d\n", (int)gfld->igdtmpl[7] );
            printf ( "  GDS bytes 35 - 38 (Ny)            = %d\n", (int)gfld->igdtmpl[8] );
            printf ( "  GDS bytes 47 - 50 (La1)           = %f\n", (float)gfld->igdtmpl[11]/1000000. );
            printf ( "  GDS bytes 51 - 54 (Lo1)           = %f\n", (float)gfld->igdtmpl[12]/1000000. );
            printf ( "  GDS byte       55 (res flag)      = %d\n", (int)gfld->igdtmpl[13] );
            printf ( "  GDS bytes 56 - 59 (La2)           = %f\n", (float)gfld->igdtmpl[14]/1000000. );
            printf ( "  GDS bytes 60 - 63 (Lo2)           = %f\n", (float)gfld->igdtmpl[15]/1000000. );
            printf ( "  GDS bytes 64 - 67 (Di)            = %f\n", (float)gfld->igdtmpl[16]/1000000. );
            printf ( "  GDS bytes 68 - 71 (Dj)            = %f\n", (float)gfld->igdtmpl[17]/1000000. );
            printf ( "  GDS byte       72 (mode)          = %d\n", (int)gfld->igdtmpl[18] );
            break;
         case 10:             /* Mercator   */
            printf ( "  GDS bytes 31 - 34 (Nx)            = %d\n", (int)gfld->igdtmpl[7] );
            printf ( "  GDS bytes 35 - 38 (Ny)            = %d\n", (int)gfld->igdtmpl[8] );
            printf ( "  GDS bytes 39 - 42 (La1)           = %f\n", (float)gfld->igdtmpl[9]/1000000. );
            printf ( "  GDS bytes 43 - 46 (Lo1)           = %f\n", (float)gfld->igdtmpl[10]/1000000. );
            printf ( "  GDS byte       47 (res flag)      = %d\n", (int)gfld->igdtmpl[11] );
            printf ( "  GDS bytes 48 - 51 (LaD)           = %f\n", (float)gfld->igdtmpl[12]/1000000. );
            printf ( "  GDS bytes 52 - 55 (La2)           = %f\n", (float)gfld->igdtmpl[13]/1000000. );
            printf ( "  GDS bytes 56 - 59 (Lo2)           = %f\n", (float)gfld->igdtmpl[14]/1000000. );
            printf ( "  GDS bytes 24 - 26 (Latin)         = %f\n", (float)gfld->igdtmpl[15]/1000000. );
            printf ( "  GDS byte       60 (scan mode)     = %d\n", (int)gfld->igdtmpl[15] );
            printf ( "  GDS bytes 61 - 64 (Orientation)   = %d\n", (int)gfld->igdtmpl[16] );
            printf ( "  GDS bytes 65 - 68 (Di)            = %f km\n", (float)gfld->igdtmpl[17]/1000000. );
            printf ( "  GDS bytes 69 - 72 (Dj)            = %f km\n", (float)gfld->igdtmpl[18]/1000000. );
            break;
         case 20:             /* Polar Stereographic */
            printf ( "  GDS bytes 31 - 34 (Nx)            = %d\n", (int)gfld->igdtmpl[7] );
            printf ( "  GDS bytes 35 - 38 (Ny)            = %d\n", (int)gfld->igdtmpl[8] );
            printf ( "  GDS bytes 39 - 42 (La1)           = %f\n", (float)gfld->igdtmpl[9]/1000000. );
            printf ( "  GDS bytes 43 - 46 (Lo1)           = %f\n", (float)gfld->igdtmpl[10]/1000000. );
            printf ( "  GDS byte       47 (res flag1)     = %d\n", (int)gfld->igdtmpl[11] );
            printf ( "  GDS bytes 48 - 51 (LaD)           = %f\n", (float)gfld->igdtmpl[12]/1000000. );
            printf ( "  GDS bytes 52 - 55 (LoV)           = %f\n", (float)gfld->igdtmpl[13]/1000000. );
            printf ( "  GDS bytes 56 - 59 (Dx)            = %f km\n", (float)gfld->igdtmpl[14]/1000000. );
            printf ( "  GDS bytes 60 - 63 (Dy)            = %f km\n", (float)gfld->igdtmpl[15]/1000000. );
            printf ( "  GDS byte       64 (proj flag2)    = %d\n", (int)gfld->igdtmpl[16] );
            printf ( "  GDS byte       65 (scan_mode)     = %d\n", (int)gfld->igdtmpl[17] );
            break;
         case 30:             /* Lambert Conformal */
            printf ( "  GDS bytes 31 - 34 (Nx)            = %d\n", (int)gfld->igdtmpl[7] );
            printf ( "  GDS bytes 35 - 38 (Ny)            = %d\n", (int)gfld->igdtmpl[8] );
            printf ( "  GDS bytes 39 - 42 (La1)           = %f\n", (float)gfld->igdtmpl[9]/1000000. );
            printf ( "  GDS bytes 43 - 46 (Lo1)           = %f\n", (float)gfld->igdtmpl[10]/1000000. );
            printf ( "  GDS byte       47 (res flag1)     = %d\n", (int)gfld->igdtmpl[11] );
            printf ( "  GDS bytes 48 - 51 (LaD)           = %f\n", (float)gfld->igdtmpl[12]/1000000. );
            printf ( "  GDS bytes 52 - 55 (LoV)           = %f\n", (float)gfld->igdtmpl[13]/1000000. );
            printf ( "  GDS bytes 56 - 59 (Dx)            = %f km\n", (float)gfld->igdtmpl[14]/1000000. );
            printf ( "  GDS bytes 60 - 63 (Dy)            = %f km\n", (float)gfld->igdtmpl[15]/1000000. );
            printf ( "  GDS byte       64 (proj flag2)    = %d\n", (int)gfld->igdtmpl[16] );
            printf ( "  GDS byte       65 (mode)          = %d\n", (int)gfld->igdtmpl[17] );
            printf ( "  GDS bytes 66 - 69 (Latin1)        = %f\n", (float)gfld->igdtmpl[18]/1000000. );
            printf ( "  GDS bytes 70 - 73 (Latin2)        = %f\n", (float)gfld->igdtmpl[19]/1000000. );
            break;
         default:
            break;
        }
    } 
    printf ( "\n" );

    /*
     *   Print out Section 4, if requested
     */
    if ( (gdflag & G2_PDS) == G2_PDS )  {
        printf ( " SECTION 4 - PRODUCT DEFINITION SECTION\n");
        printf ( "  PDS bytes  6 -  7 (no. of coords) = %d\n", (int)gfld->num_coord );
        printf ( "  Product Definition Template 4.%d: ",(int)gfld->ipdtnum);
        for (i = 0; i < gfld->ipdtlen; i++ )
                printf ( " %d",(int)gfld->ipdtmpl[i]);
        printf ( "\n\n" );
    } 

    /*
     *   Print out Section 5, if requested
     */
    if ( (gdflag & G2_DRS) == G2_DRS )  {
        printf ( " SECTION 5 - DATA REPRESENTATION SECTION\n");
        printf ( "  DRS bytes  6 -  9 (no. of data points) = %d\n", (int)gfld->ndpts );
        printf ( "  Data Representation Template 5.%d: ",(int)gfld->idrtnum);
        for (i = 0; i < gfld->idrtlen; i++ )
                printf ( " %d",(int)gfld->idrtmpl[i]);
        printf ( "\n\n" );
    } 

    /*
     *   Print out Section 6, if requested
     */
    if ( (gdflag & G2_BMS) == G2_BMS )  {
        printf ( " SECTION 6 - BIT-MAP SECTION\n");
        printf ( "  BMS byte        6 (bit-map indicator) = %d\n\n", (int)gfld->ibmap );
    } 

}
