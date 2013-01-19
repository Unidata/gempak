#include "nsncmn.h"

void dumpcmn ( int *iret );

int main ( void )
/************************************************************************
 * TESTNSN								*
 *									*
 * This program test the NSN library of routines.			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * M. Li/GSC		 7/00	Added nsn_save and nsn_rest		*
 * T. Lee/SAIC		 8/03	Add time interval to nsn_gtim, nsn_dspl	*
 * T. Lee/SAIC		 2/04	Add reference time flag to nsn_gtim	*
 * T. Lee/SAIC		 4/04	Added delta reference time to nsn_gtim	*
 * T. Lee/SAIC		10/04	Added bin hours				*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 * F. J. Yen/NCEP	04/08   Insert new parms for nsn_dspl (CSC).	*
 *				Request input for bin hours.		*
 ***********************************************************************/
{
	int	cont, ier, iret, numsub, id;
	char	ergrp[4], erstr[81], select[LLSCRN];

	int	iindex, jindex, knt, ntime, match, ititl, idelta,
		mode, istat, minute, isbcat, mrange, intrvl,
		ibfr, iaftr, mbfr, maftr, mstrct;
	char	alias[81], cycle[81], parms[81], color[81],
		level[81], vcord[81], filter[81], txtatt[81],
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

	strcpy ( ergrp, "NSN" );

	cont = G_TRUE;

	while ( cont ) {
	    printf ( "\n\n" );
	    printf ( "   1 = NSN_INIT   2 = NSN_SATT   3 = NSN_QATT\n" );
	    printf ( "   4 = NSN_GTIM   5 = NSN_DSPL   6 = NSN_SAVE\n" );
	    printf ( "   7 = NSN_REST\n\n" );
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
		nsn_init ( &iret );

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
		printf ( "Enter the data alias (e.g., UAIR):\n" );
		scanf  ( " %s", alias );
		printf ( "Enter data subcategory number:\n" );
		scanf  ( " %d", &isbcat );
		printf ( "Enter the cycle date/time or NONE:\n" );
		scanf  ( " %s", cycle );
		printf ( "Enter the parm list:\n" );
		scanf  ( " %s", parms );
		printf ( "Enter the color list:\n" );
		scanf  ( " %s", color );
		printf ( "Enter the level:\n" );
		scanf  ( " %s", level );
		printf ( "Enter the vertical coordinate:\n" );
		scanf  ( " %s", vcord );
		printf ( "Enter the filter:\n" );
		scanf  ( " %s", filter );
		printf ( "Enter the text attributes string:\n" );
		scanf  ( " %s", txtatt );

		nsn_satt ( iindex, alias, isbcat, cycle, parms, color,
			   level, vcord, filter, txtatt, &jindex,
			   &iret );

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

		nsn_qatt ( iindex, alias, &isbcat, cycle, parms, color,
			   level, vcord, filter, txtatt, &iret );

		printf ( "iret   = %d\n", iret );
		printf ( "alias  = %s\n", alias );
		printf ( "isbcat = %d\n", isbcat );
		printf ( "cycle  = %s\n", cycle );
		printf ( "parms  = %s\n", parms );
		printf ( "color  = %s\n", color );
		printf ( "level  = %s\n", level );
		printf ( "vcord  = %s\n", vcord );
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
		scanf  ( " %d", &intrvl );
		printf ( "Enter reference time flag:\n" );
		scanf  ( " %u", &jflag );
		iflag = (Boolean) jflag;
		printf ( "Enter delta reference time in minutes:\n" );
		scanf  ( " %d", &idelta );


		nsn_gtim ( iindex, endtim, mrange, intrvl, iflag, 
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

		nsn_qatt ( iindex, alias, &isbcat, cycle, parms, color,
			   level, vcord, filter, txtatt, &iret );

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
                printf ( "Enter binning time before current time: hh mm:\n" );
                scanf  ( " %d %d", &ibfr, &mbfr );
                printf ( "Enter binning time after current time: hh mm:\n" );
                scanf  ( " %d %d", &iaftr, &maftr );
                printf ( "Enter most recent only flag (0 for no; 1 for yes)\n" );
                scanf  ( " %d", &mstrct );

		gg_maps ( proj, garea, blank, &id, &ier,
			  strlen ( proj ), strlen ( garea ),
			  strlen ( blank ) );

		gclear ( &ier );

		strcpy ( map, "1" );
		gg_map ( map, &ier, strlen ( map ) );

		strcpy ( ltln, "2" );
		gg_ltln ( ltln, &ier, strlen ( ltln ) );

		nsn_dspl ( panel, dattim, alias, &isbcat, cycle, parms,
			   color, level, vcord, filter, txtatt,       
			   endtim, &mrange, &intrvl, &match, &minute, 
			   &ititl, &ibfr, &mbfr, &iaftr, &maftr,
			   &mstrct, &iret,
			   strlen ( panel ), strlen ( dattim ),
			   strlen ( alias ), strlen ( cycle ),
			   strlen ( parms ), strlen ( color ),
			   strlen ( level ), strlen ( vcord ),
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
                nsn_save ( &iret );

                printf ( "iret = %d\n", iret );

                if  ( iret != 0 )  {
                    strcpy ( erstr, " " );
                    er_wmsg ( ergrp, &iret, erstr, &ier,
                              strlen ( ergrp ), strlen ( erstr ) );
                }
            }

/*---------------------------------------------------------------------*/
            if  ( numsub == 7 )  {
                nsn_rest ( &iret );

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
 * This routine dumps the contents of the NSN common variables to the	*
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
 * T. Piper/SAIC        06/03   Replaced nsnd with MAXTMPLT             *
 ***********************************************************************/
{

	int		i;

/*---------------------------------------------------------------------*/

	*iret = 0;

	for  ( i = 0; i < MAXTMPLT; i++ )  {

	     printf ( "\nUpper Air Data Attributes: index = %d\n",
			indsnd[i] );
	     printf ( "    alias  = %s\n", snddt[indsnd[i]].alias );
	     printf ( "    isbcat = %d\n", snddt[indsnd[i]].isbcat );
	     printf ( "    cycle  = %s\n", snddt[indsnd[i]].cycle );
	     printf ( "    parms  = %s\n", snddt[indsnd[i]].parms );
	     printf ( "    color  = %s\n", snddt[indsnd[i]].color );
	     printf ( "    level  = %s\n", snddt[indsnd[i]].level );
	     printf ( "    vcord  = %s\n", snddt[indsnd[i]].vcord );
	     printf ( "    filter = %s\n", snddt[indsnd[i]].filter );
	     printf ( "    txtatt = %s\n", snddt[indsnd[i]].txtatt );

	}

}
