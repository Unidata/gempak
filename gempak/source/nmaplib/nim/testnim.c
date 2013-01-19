#include "nimcmn.h"

void dumpcmn ( int *iret );

int main ( void )
/************************************************************************
 * TESTNIM								*
 *									*
 * This program test the NIM library of routines.			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Jacobs/NCEP	10/99	Added NIM_GFIL				*
 * M. Li/GSC		 7/00	Added NIM_SAVE and NIM_REST		*
 * T. Lee/SAIC		 8/03	Add time interval to nim_gtim, nim_dspl,*
 *				and nim_gfil				*
 * T. Lee/SAIC		02/04	Added jflag, jauto, basetm to nim_gtim	*
 * T. Piper/SAIC	04/07	Removed NIM_DSPL; mod for nim_gtim CSC	*
 ***********************************************************************/
{
	int	cont, ier, iret, numsub;
	char	ergrp[4], erstr[81], select[LLSCRN];

	int	iindex, jindex, mrange, intrvl, knt, ntime, match, 
		mode, istat, minute;
	char	imtype[4], iminfo[81], imlutf[81],
		dattim[21], device[81], filnam[LLPATH];
	dttms_t	endtim, basetm;
	unsigned jflag, jauto;
	Boolean	iflag, iauto;
	char **timarr=NULL;
/*---------------------------------------------------------------------*/

	in_bdta ( &ier );

	mode = 1;
	ginitp ( &mode, &istat, &ier );

	printf ( "Enter full DEVICE string:\n" );
	scanf ( " %s", device );

	gg_sdev ( device, &ier, strlen ( device ) );

	strcpy ( ergrp, "NIM" );

	cont = G_TRUE;

	while ( cont ) {
	    printf ( "\n\n" );
	    printf ( "   1 = NIM_INIT   2 = NIM_SATT   3 = NIM_QATT\n" );
	    printf ( "   4 = NIM_GTIM   5 =            6 = NIM_GFIL\n" );
	    printf ( "   7 = NIM_SAVE   8 = NIM_REST \n\n" );
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
		nim_init ( &iret );

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
		printf ( "Enter the type of image (SAT/RAD):\n" );
		scanf  ( " %s", imtype );
		printf ( "Enter the image directory info:\n" );
		scanf  ( " %s", iminfo );
		printf ( "Enter the color LUT file name:\n" );
		scanf  ( " %s", imlutf );

		nim_satt ( iindex, imtype, iminfo, imlutf,
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

		nim_qatt ( iindex, imtype, iminfo, imlutf, &iret );

		printf ( "iret   = %d\n", iret );
		printf ( "imtype = %s\n", imtype );
		printf ( "iminfo = %s\n", iminfo );
		printf ( "imlutf = %s\n", imlutf );

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
		printf ( "Enter reference time flag (0 or 1):\n" );
		scanf  ( " %u", &jflag );
		printf ( "Enter auto update flag (0 or 1):\n" );
		scanf  ( " %u", &jauto );
		printf ( "Enter base time:\n" );
		scanf  ( " %s", basetm );

		nim_gtim ( iindex, endtim, mrange, intrvl, iflag, iauto,
			   basetm, &ntime, &timarr, &iret );

		printf ( "iret  = %d\n", iret );
		printf ( "ntime = %d\n", ntime );
		if  ( ntime > 0 )  {
		    for ( knt = 0; knt < ntime; knt++ ) {
			printf ( "timarr[%d] = %s\n", knt, timarr[knt] );
			G_FREE(timarr[knt], char);
		    }
		    G_FREE(timarr, char*);
		}

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 5 )  {
	    }

/*---------------------------------------------------------------------*/
	    if  ( numsub == 6 )  {
		printf ( "Enter index number:\n" );
		scanf  ( " %d", &iindex );

		printf ( "Enter DATTIM:\n" );
		scanf  ( " %s", dattim );

		printf ( "Enter end time for range:\n" );
		scanf  ( " %s", endtim );
		printf ( "Enter the time range in minutes:\n" );
		scanf  ( " %d", &mrange );
		printf ( "Enter the time interval in minutes:\n" );
		scanf  ( " %d", &intrvl );

		printf ( "Enter time match type:\n" );
		scanf  ( " %d", &match );
		printf ( "Enter minutes for difference match:\n" );
		scanf  ( " %d", &minute );

		nim_gfil ( iindex, dattim, endtim, mrange, intrvl, 
			   match, minute, filnam, &iret );

		printf ( "filnam = %s\n", filnam );

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}

	    }

/*---------------------------------------------------------------------*/
           if  ( numsub == 7 )  {
                nim_save ( &iret );

                printf ( "iret = %d\n", iret );

                if  ( iret != 0 )  {
                    strcpy ( erstr, " " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }

/*---------------------------------------------------------------------*/
           if  ( numsub == 8 )  {
                nim_rest ( &iret );

                printf ( "iret = %d\n", iret );

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

/*====================================================================*/

void dumpcmn ( int *iret )
/************************************************************************
 * dumpcmn								*
 *									*
 * This routine dumps the contents of the NIM common variables to the	*
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
 * T. Piper/SAIC	06/03	Replaced nimg with MAXTMPLT		*
 ***********************************************************************/
{

	int		i;

/*---------------------------------------------------------------------*/

	*iret = 0;

	for  ( i = 0; i < MAXTMPLT; i++ )  {

	     printf ( "\nImage Data Attributes: index = %d\n",
			indimg[i] );
	     printf ( "    type = %s\n", image[indimg[i]].type );
	     printf ( "    info = %s\n", image[indimg[i]].info );
	     printf ( "    lutf = %s\n", image[indimg[i]].lutf );

	}

}
