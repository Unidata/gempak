#include "nsfcmn.h"

void dumpcmn ( int *iret );

int main ( void )
/************************************************************************
 * TESTNSF								*
 *									*
 * This program test the NSF library of routines.			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * M. Li/GSC		 7/00	Added NSF_SAVE and NSF_REST		*
 * T. Lee/SAIC		 8/03	Add time interval to nsf_gtim, nsf_dspl	*
 * T. Lee/SAIC		 2/04	Add reference time flag to nsf_gtim	*
 * T. Lee/SAIC		 4/04	Added delta reference time to nsf_gtim	*
 * T. Lee/SAIC		10/04	Added bin hours				*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 * F. J. Yen/NCEP	04/08	CSC to nsf_dspl (minutes & most recent).*
 *				Request input for bin hours.		*
 ***********************************************************************/
{
	int	cont, ier, iret, numsub, id;
	char	ergrp[4], erstr[81], select[LLSCRN];

	int	iindex, jindex, knt, ntime, match, ititl, idelta,
		mode, istat, minute, isbcat, mrange, intrvl, 
		ibfr, iaftr, mbfr, maftr, mstrct;
	char	alias[81], cycle[81], parms[81], color[81], filter[81],
		txtatt[81],
		garea[81], proj[5], panel[81], dattim[21], device[81],
		map[21], ltln[21], ans[9];
	unsigned int	jflag;
	Boolean	iflag;
	dttms_t	endtim, timarr[2000];

	char	blank[] = " ";

/*---------------------------------------------------------------------*/

	in_bdta ( &ier );
	gd_init ( &ier );
	mode = 1;
	ginitp ( &mode, &istat, &ier );

	printf ( "Enter full DEVICE string:\n" );
	scanf ( " %s", device );

	gg_sdev ( device, &ier, strlen ( device ) );

	strcpy ( ergrp, "NSF" );

	cont = G_TRUE;

	while ( cont ) {
	    printf ( "\n\n" );
	    printf ( "   1 = NSF_INIT   2 = NSF_SATT   3 = NSF_QATT\n" );
	    printf ( "   4 = NSF_GTIM   5 = NSF_DSPL   6 = NSF_SAVE\n" );
	    printf ( "   7 = NSF_REST \n\n" );
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
		nsf_init ( &iret );

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
		printf ( "Enter the data alias (e.g., METAR, NGMMOS):\n" );
		scanf  ( " %s", alias );
		printf ( "Enter data subcategory number:\n" );
		scanf  ( " %d", &isbcat );
		printf ( "Enter the cycle date/time or NONE:\n" );
		scanf  ( " %s", cycle );
		printf ( "Enter the parm list:\n" );
		scanf  ( " %s", parms );
		printf ( "Enter the color list:\n" );
		scanf  ( " %s", color );
		printf ( "Enter the filter:\n" );
		scanf  ( " %s", filter );
		printf ( "Enter the text attributes string:\n" );
		scanf  ( " %s", txtatt );

		nsf_satt ( iindex, alias, isbcat, cycle, parms, color,
			   filter, txtatt, &jindex, &iret );

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

		nsf_qatt ( iindex, alias, &isbcat, cycle, parms, color,
			   filter, txtatt, &iret );

		printf ( "iret   = %d\n", iret );
		printf ( "alias  = %s\n", alias );
		printf ( "isbcat = %d\n", isbcat );
		printf ( "cycle  = %s\n", cycle );
		printf ( "parms  = %s\n", parms );
		printf ( "color  = %s\n", color );
		printf ( "filter = %s\n", filter );
		printf ( "txtatt = %s\n", txtatt );

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
		scanf  ( " %d", &intrvl);
		printf ( "Enter reference time flag:\n" );
		scanf  ( " %u", &jflag );
		iflag = (Boolean) jflag;
		printf ( "Enter delta reference time in minutes:\n" );
		scanf  ( " %d", &idelta );

		nsf_gtim ( iindex, endtim, mrange, intrvl, iflag, 
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
		printf ( "Enter index number:\n" );
		scanf  ( " %d", &iindex );

		nsf_qatt ( iindex, alias, &isbcat, cycle, parms, color,
			   filter, txtatt, &iret );

		printf ( "Enter GAREA:\n" );
		scanf  ( " %s", garea );

		printf ( "Default projection? (y/n)\n" );
		scanf  ( " %s", ans );
		if  ( ans[0] == 'N' || ans[0] == 'n' )  {
		    printf ( "Enter PROJ:\n" );
		    scanf  ( " %s", proj );
		}
		else {
		    strcpy ( proj, blank );
		}

		printf ( "Enter PANEL:\n" );
		scanf  ( " %s", panel );
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
		printf ( "Enter title line:\n" );
		scanf  ( " %d", &ititl );
		printf ( "Enter time binning hours before current time\n" );
		scanf  ( " %d", &ibfr );
		printf ( "Enter time binning minutes before current time\n" );
		scanf  ( " %d", &mbfr );
		printf ( "Enter time binning hours after current time\n" );
		scanf  ( " %d", &iaftr );
		printf ( "Enter time binning minutes after current time\n" );
		scanf  ( " %d", &maftr );
		printf ( "Enter most recent only flag (0 for no; 1 for yes" );
		scanf  ( " %d", &mstrct );

		gg_maps ( proj, garea, blank, &id, &ier,
			  strlen ( proj ), strlen ( garea ),
			  strlen ( blank ) );

		gclear ( &ier );

		strcpy ( map, "1" );
		gg_map ( map, &ier, strlen ( map ) );

		strcpy ( ltln, "2" );
		gg_ltln ( ltln, &ier, strlen ( ltln ) );

		nsf_dspl ( panel, dattim, alias, &isbcat, cycle, parms,
			   color, filter, txtatt, endtim, &mrange, 
			   &intrvl, &match, &minute, &ititl, 
			   &ibfr, &mbfr, &iaftr, &maftr, &mstrct, &iret,
			   strlen ( panel ), strlen ( dattim ),
			   strlen ( alias ), strlen ( cycle ),
			   strlen ( parms ), strlen ( color ),
			   strlen ( filter ), strlen ( txtatt ),
			   strlen ( endtim ) );

		geplot ( &ier );

		if  ( iret != 0 )  {
		    strcpy ( erstr, " " );
		    er_wmsg ( ergrp, &iret, erstr, &ier,
			      strlen ( ergrp ), strlen ( erstr ) );
		}
	    }

/*---------------------------------------------------------------------*/
            if  ( numsub == 6 )  {
                nsf_save ( &iret );

                printf ( "iret = %d\n", iret );

                if  ( iret != 0 )  {
                    strcpy ( erstr, " " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }

/*---------------------------------------------------------------------*/
            if  ( numsub == 7 )  {
                nsf_rest ( &iret );

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

/*=====================================================================*/

void dumpcmn ( int *iret )
/************************************************************************
 * dumpcmn								*
 *									*
 * This routine dumps the contents of the NSF common variables to the	*
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
 * T. Piper/SAIC        06/03   Replaced nsfc with MAXTMPLT             *
 ***********************************************************************/
{

	int		i;

/*---------------------------------------------------------------------*/

	*iret = 0;

	for  ( i = 0; i < MAXTMPLT; i++ )  {

	     printf ( "\nSurface Data Attributes: index = %d\n",
			indsfc[i] );
	     printf ( "    alias  = %s\n", sfcdt[indsfc[i]].alias );
	     printf ( "    isbcat = %d\n", sfcdt[indsfc[i]].isbcat );
	     printf ( "    cycle  = %s\n", sfcdt[indsfc[i]].cycle );
	     printf ( "    parms  = %s\n", sfcdt[indsfc[i]].parms );
	     printf ( "    color  = %s\n", sfcdt[indsfc[i]].color );
	     printf ( "    filter = %s\n", sfcdt[indsfc[i]].filter );
	     printf ( "    txtatt = %s\n", sfcdt[indsfc[i]].txtatt );

	}

}
