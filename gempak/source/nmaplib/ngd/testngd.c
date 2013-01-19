#include "ngdcmn.h"

void dumpcmn ( int *iret );

int main ( void )
/************************************************************************
 * TESTNGD								*
 *									*
 * This program test the NGD library of routines.			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * M. Li/GSC		 7/00	Added ngd_save and ngd_rest		*
 * S. Jacobs/NCEP	 3/01	Added ngd_gnms				*
 * S. Jacobs/NCEP	 7/01	Added ngd_rsfl				*
 * S. Jacobs/NCEP	 9/01	Make main program a void function	*
 * T. Lee/SAIC		 8/03	Add time interval to ngd_gtim, ngd_dspl	*
 * T. Lee/SAIC		 2/04	Add reference time flag to ngd_gtim	*
 * T. Lee/SAIC		 4/04	Add delta reference time to ngd_gtim	*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 ***********************************************************************/
{
	int	cont, ier, iret, numsub;
	char	ergrp[4], erstr[81], select[LLSCRN];

	int	iindex, jindex, knt, ntime, idelta,
		mode, istat, isbcat, mrange, intrvl, nname;
	char	alias[81], cycle[81], rstfil[81], device[81];
	unsigned int	jflag;
	Boolean	iflag;
	dattm_t	endtim, timarr[2000];

	nmlst_t	namarr[MXNMFL];

/*---------------------------------------------------------------------*/

	in_bdta ( &ier );
	gd_init ( &ier );

	mode = 1;
	ginitp ( &mode, &istat, &ier );

	printf ( "Enter full DEVICE string:\n" );
	scanf ( " %s", device );

	gg_sdev ( device, &ier, strlen ( device ) );

	strcpy ( ergrp, "NGD" );

	cont = G_TRUE;

	while ( cont ) {
	    printf ( "\n\n" );
	    printf ( "   1 = NGD_INIT   2 = NGD_SATT   3 = NGD_QATT\n" );
	    printf ( "   4 = NGD_GTIM   5 = NGD_SAVE\n" );
	    printf ( "   6 = NGD_REST   7 = NGD_GNMS\n\n" );
	    printf ( "  10 = Dump common\n\n" );
	    printf ( "  20 = Change device\n\n" );
	    printf ( "\n" );
	    printf ( "Select a subroutine number or type EXIT: " );
	    scanf ( " %s", select );
	    switch ( select[0] ) {
		case 'e':
		case 'E':
			cont = G_FALSE;
		default:
			numsub = atoi ( select );
			break;
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 1 )  {
		ngd_init ( &iret );

		printf ( "iret = %d\n", iret );

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 2 )  {
		printf ( "Enter index number:\n" );
		scanf  ( " %d", &iindex );
		printf ( "Enter the data alias (e.g., ETA, RUCS):\n" );
		scanf  ( " %s", alias );
		printf ( "Enter data subcategory number:\n" );
		scanf  ( " %d", &isbcat );
		printf ( "Enter the cycle date/time or NONE:\n" );
		scanf  ( " %s", cycle );
		printf ( "Enter the full path for the restore file:\n" );
		scanf  ( " %s", rstfil );

		ngd_satt ( iindex, alias, isbcat, cycle, rstfil,
			   &jindex, &iret );

		printf ( "iret   = %d\n", iret );
		printf ( "jindex = %d\n", jindex );

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 3 )  {
		printf ( "Enter index number:\n" );
		scanf  ( " %d", &iindex );

		ngd_qatt ( iindex, alias, &isbcat, cycle, rstfil,
			   &iret );

		printf ( "iret   = %d\n", iret );
		printf ( "alias  = %s\n", alias );
		printf ( "isbcat = %d\n", isbcat );
		printf ( "cycle  = %s\n", cycle );
		printf ( "rstfil = %s\n", rstfil );

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 4 )  {
		printf ( "Enter index number:\n" );
		scanf  ( " %d", &iindex );
                printf ( "Enter end time for range:\n" );
		scanf  ( " %s", endtim );
		printf ( "Enter time range in minutes:\n" );
		scanf  ( " %d", &mrange );
		printf ( "Enter time interval in minutes:\n" );
		scanf  ( " %d", &intrvl );
		printf ( "Enter reference time flag:\n" );
		scanf  ( " %u", &jflag );
 		iflag = (Boolean) jflag;
		printf ( "Enter delta reference time:\n" );
		scanf  ( " %d", &idelta );
		
		ngd_gtim ( iindex, endtim, mrange, intrvl, iflag, 
			   &idelta, &ntime, timarr, &iret );

		printf ( "idelta = %d\n", idelta );
		printf ( "iret   = %d\n", iret );
		printf ( "ntime  = %d\n", ntime );
		if  ( ntime > 0 )  {
		    for ( knt = 0; knt < ntime; knt++ ) {
			printf ( "Times: timarr[%d] = %s\n",
				 knt, timarr[knt] );
		    }
		}

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 5 )  {
                ngd_save ( &iret );

                printf ( "iret = %d\n", iret );

                if  ( iret != 0 )  {
                    strcpy ( erstr, " " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }

/*---------------------------------------------------------------------*/
            if  ( numsub == 6 )  {
                ngd_rest ( &iret );

                printf ( "iret = %d\n", iret );

                if  ( iret != 0 )  {
                    strcpy ( erstr, " " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 7 )  {
		printf ( "Enter the data alias (e.g., ETA, RUCS):\n" );
		scanf  ( " %s", alias );

		ngd_gnms ( alias, &nname, namarr, &iret );

		printf ( "iret  = %d\n", iret );
		printf ( "nname = %d\n", nname );
		if  ( nname > 0 )  {
		    for ( knt = 0; knt < nname; knt++ ) {
			printf ( "Names: namarr[%d] = %s\n",
				 knt, namarr[knt] );
		    }
		}

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    	
            }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 10 )  {
		dumpcmn ( &iret );
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 20 )  {
		printf ( "Enter full DEVICE string:\n" );
		scanf ( " %s", device );

		gg_sdev ( device, &ier, strlen ( device ) );
	    }

	}
	return(0);
}

void dumpcmn ( int *iret )
/************************************************************************
 * dumpcmn								*
 *									*
 * This routine dumps the contents of the NGD common variables to the	*
 * screen.								*
 *									*
 * dumpcmn ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * T. Piper/SAIC        06/03   Replaced ngrd with MAXTMPLT             *
 ***********************************************************************/
{

	int		i;

/*---------------------------------------------------------------------*/

	*iret = 0;

	for  ( i = 0; i < MAXTMPLT; i++ )  {

	     printf ( "\nGrid Data Attributes: index = %d\n",
			indgrd[i] );
	     printf ( "    alias  = %s\n", grddt[indgrd[i]].alias );
	     printf ( "    isbcat = %d\n", grddt[indgrd[i]].isbcat );
	     printf ( "    cycle  = %s\n", grddt[indgrd[i]].cycle );
	     printf ( "    rstfil = %s\n", grddt[indgrd[i]].rstfil );

	}

}
