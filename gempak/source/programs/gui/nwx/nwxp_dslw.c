#include "nwx_cmn.h"


/************************************************************************
 * nwxp_dslw.c                                                          *
 *                                                                      *
 * This processing module applys and loads new data selections.         *
 *                                                                      *
 * CONTENTS:                                                            *
 *      dslw_apply()    apply the new data selection(s)                 *
 * 	dslw_load() 	load the data for the new data selection(s)	*	
 * 	dslw_srchMstr()	search master list for requested data type	*
 ***********************************************************************/

/*=====================================================================*/

void dslw_apply ( void ) 
/************************************************************************
 * dslw_apply                                                           *
 *                                                                      *
 * This routine is processing function to apply the new data selection. *
 *                                                                      *
 * dslw_apply ( )                                            		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			None						*
 *                                                                      *
 **                                                                     *
 * E. Safford/SAIC      12/07	split off from dslw.c, use err_showError*
 ***********************************************************************/
{
int ier;
char emptyStr[2] = "";
/* ------------------------------------------------------------------- */

/*
 *  Clear text window
 */
    pdata_setReportText( emptyStr, &ier ); 
    if( ier >= G_NORMAL ) {
        txtw_dsplyReport( &ier );
    }

    pdata_setDateTime( " ", &ier );
    txtw_dsplyDateTime( &ier );

/*
 *  Load data by user click.
 */
    dslw_load ( 1, &ier );
    if ( ier < -7 ) {
	err_showError( ier ); 
    }
}

/*=====================================================================*/

void dslw_load ( int called_from, int *iret )
/************************************************************************
 * dslw_load                                                            *
 *                                                                      *
 * This routine is the callback for the data type menu.                 *
 *                                                                      *
 * dslw_load ( called_from, iret ) 			                *
 *                                                                      *
 * Input parameters:                                                    *
 *	called_from	int	Indicate from where it is called	*
 *				  1 - from user click			*
 *				  2 - from auto-update			*
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					-8 -- scandir failure		*
 *					-9 -- error opening file	*
 *                                                                      *
 **                                                                     *
 * R. Tian/SAIC		11/03	Separated from dslw_apply() and added	*	
 *				auto_startAutoUpdt()			*	
 * R. Tian/SAIC		12/03	Redraw the selected station markers and	*	
 *				auto-update the text window		*
 * T. Piper/SAIC	01/04	added return if nwxtbl_getstns fails	*
 * T. Piper/SAIC	01/04	Added check for < -7 from fosd_getdata	*
 * T. Piper/SAIC	01/04	added return if fosd_txtrd fails	*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 * E. Safford/SAIC	12/07	split off from dslw.c, use txtw_PrevNext*
 * L. Hinson/AWC        05/09   Fix problem with Text Update            *
 ***********************************************************************/
{
int             jcolr, imrk, ier, ier1;
int             itime[5], isec, jday;
int             itype;
char            *datatype, *bsflag, tmzn[4]; 
char		report[ REPMAX ], fullReport[ REPMAX ];
int		minutes;
char            srchstr[LLSTFL][SRCHSTR_LEN], nodata_msg[80]="\0";
int		ii, stnValue;
/*---------------------------------------------------------------------*/
	*iret = 0;

/*
 * Now that it is in this routine, make sure NO timeout exists,
 * and display a busy cursor if it is in auto-update.
 */
	 auto_stopAutoUpdt();

/*
 * Get the current date and time.
 */
	itype = 1;
        css_date ( &itype, &itime[0], &itime[1], &itime[2], &itime[3],
                   &itime[4], &isec, &jday, tmzn, &ier );

/*
 * Draw the map.
 */
        draw_map(usrSelect.mapindx, nwxTable->map_info,
                        (int)usrSelect.zoomflg, &mapBnd, &ier);

/*
 * Set up the product and group name on the text window
 */
        txtw_prdgrpSet();

/*
 * set data type index, search flag, and data type string
 */
        if ( srchInfo.idtyp < 0 ) {

/*
 * Product unavailable
 */
	   if ( called_from == 1 ) {
               strcpy(fullReport, "Product not selected or unavailable.\n");
  	       pdata_setReportText( fullReport, &ier1 );
               txtw_dsplyReport( &ier1 );
	   }

	   auto_startAutoUpdt();
           return;
        }

	usrSelect.prvnxt = 0;
        srchInfo.sflag = 0;
        datatype = nwxTable->dtyp_info[srchInfo.idtyp].datatyp;
        bsflag   = nwxTable->dtyp_info[srchInfo.idtyp].bsflag;

/*
 * set the time search criteria and srchInfo.smethod
 */
	srchInfo.smethod = STANDARD;
	if ( bsflag[0] == 'W' || bsflag[0] == 'O' ) {
	    usrSelect.ndttm = usrSelect.ddttm;
		if ( bsflag[0] == 'W' )  srchInfo.smethod = WATCHWARN;
		if ( bsflag[0] == 'O' )  srchInfo.smethod = OBS;
	}
	else  {
		usrSelect.ndttm = 120;
	}

/*
 * set the starting/ending time in search structure
 */
	srchInfo.endd.year  = itime[0];
	srchInfo.endd.month = itime[1];
	srchInfo.endd.day   = itime[2];
	srchInfo.endd.hour  = itime[3];

	minutes = 60 * usrSelect.ndttm;
	ti_subm( itime, &minutes, itime, &ier );

	srchInfo.startd.year  = itime[0];
	srchInfo.startd.month = itime[1];
	srchInfo.startd.day   = itime[2];
	srchInfo.startd.hour  = itime[3];

/*
 * Read the location table for either station data or
 * bulletin data.
 */
        nwxtbl_getstns( srchInfo.idtyp, &stnList, &ier );
	if ( ier != 0 ) {
	    *iret = -9;
	    return;
	}

/*
 * set plotData.mode
 */
        if ( bsflag[0] == 'Z' ) {

              plotData.mode = VALUE;

        }
        else {
              if  ( stnList.nstn == 1 ) {

                  if (strcmp(datatype, "QPF_DISC") == 0 ) {
                      plotData.mode = GRAPHIC;
                  }
                  else if (strcmp(datatype, "WATCH_BOX") == 0 ) {
                      plotData.mode = WATCHBOX;
                  }
		  else
		      plotData.mode = EMPTY;

	      };

	     if ( stnList.nstn > 1 )
		  plotData.mode = STNSELECT;

	}

/*
 * set prev/next buttons 
 */
	if ( plotData.mode == STNSELECT || plotData.mode == WATCHBOX ) {
	    txtw_setPrevNext( G_FALSE, G_FALSE );
	}
	else {
	    txtw_setPrevNext( G_TRUE, G_TRUE );
	}

/*
 * if multi-stations, draw the station markers for
 * further selection
 */

	if  ( plotData.mode == STNSELECT  ) {

	    jcolr = ALL_COL;
	    imrk  = ALL_MRK;

	    if ( srchInfo.smethod == WATCHWARN ) {

/*
 * scan the text data files first to get the station list
 */
		srchw_fosdScan( &srchInfo, called_from, iret);
		if ( *iret < -7 ) {
		    return;
		}
		draw_stnmark( stnList.nrptstn, stnList.rptstnlat,
			      stnList.rptstnlon, jcolr, imrk, &ier );

	    }
	    else if ( srchInfo.smethod == STANDARD ||
	              srchInfo.smethod == OBS ) {

/*
 * Plot the station markers for select.
 */
		draw_stnmark( stnList.nstn, stnList.lat, stnList.lon,
			      jcolr, imrk, &ier );

/*
 * When called from auto-update, redraw the selected station
 * markers and update the text window.
 */
	        if ( called_from == 2 ) {
		    jcolr = SEL_COL;
		    imrk  = SEL_MRK;
		    draw_stnmark( plotData.plt_mark.nstn, 
		                  plotData.plt_mark.lat,
		                  plotData.plt_mark.lon, jcolr, imrk, &ier );

		    for ( ii = 0; ii < plotData.plt_mark.nstn; ii++ ) {
		        stnValue = mapw_getStation( ii );
		        strcpy(srchstr[ii], stnList.srchstr[stnValue]);
		    }
		    qsort(srchstr, plotData.plt_mark.nstn, SRCHSTR_LEN,
		          (int(*)(const void*, const void*))strcmp);
		    for ( ii = 0; ii < plotData.plt_mark.nstn; ii++ ) {
		        srchInfo.sflag = 0;
			strcpy( srchInfo.srchstr, srchstr[ii] );
			fosd_getdata( nwxTable->dtyp_info, &(srchInfo), 1,
			              report, &ier );
/* Possible problem by not restarting auto-update */
			if ( ier < -7 ) return;
/*
 * If the report is empty
 */
                        if ( !report[0] ) {
/*
 * if selected by station, deactivate prev/next button
 */
                            if ( ii == 0 ) {
			        pdata_setDateTime( " ", &ier1 );
				txtw_dsplyDateTime( &ier1 );

                                if ( usrSelect.selct_by == STATION ) {
	                            txtw_setPrevNext( G_FALSE, G_FALSE );
                                }
                            }
                            sprintf(nodata_msg,
                            "No data available for %s in the last %d hour(s).\n",
                            srchInfo.srchstr, usrSelect.ndttm );
                            if ( ii == 0 ) {
                              strcpy( fullReport, nodata_msg ); 
                              strcpy( printText, nodata_msg);
                            }
                            else {
                              strcat( fullReport, nodata_msg);
                              strcat( printText, nodata_msg);
                            }
                        }  else {

/*
 * Display the text in the text window.
 */
                           if  ( ii == 0 ) {
			       strcpy( fullReport, report );
                               strcpy( printText, report );
                           }
                           else {
			       strcat( fullReport, report );
                               strcat( printText, report );
                           }
              
                        }
                        if ( usrSelect.selct_by == STATE ) {
			       strcat( fullReport, "----------\n" );
                               strcat( printText, "----------\n" );
                        }
		    }
		    pdata_setReportText( fullReport, &ier1 );
                    txtw_dsplyReport( &ier1 );

		    jcolr = ALL_COL;
		    imrk  = ALL_MRK;
		}
	    }
	}
	else if  ( plotData.mode == WATCHBOX )  {

/*
 * Decode and plot current watches.
 */
	    fosd_wbox ( called_from );

	}
	else {

/*
 * read report in to reportText and display the text
 */
	    fosd_txtrd( called_from );

/*
 * decode the data
 */
	    fosd_decode();

/*
 * plot the data
 */
	    fosd_plot( called_from );

	}

/*
 * Restore the cursor if it is busy and start auto-update.
 */
	auto_startAutoUpdt();

}

/*=====================================================================*/

void dslw_srchMaster ( int item, int *iret ) 
/************************************************************************
 * dslw_srchMaster                                                      *
 *                                                                      *
 * This routine searches the master table for the requested data type.  *
 *                                                                      *
 * dslw_srchMaster ( )                                         		*
 *                                                                      *
 * Input parameters:                                                    *
 *	item		int	item number from gui list		*
 *									*
 * Output parameters:                                                   *
 *	iret		*int	return code				*
 *				   0 = Normal				*
 **                                                                     *
 * E. Safford/SAIC      12/07	split off from dslw_prodCb()            *
 ***********************************************************************/
{
int 	ier, index;
char 	*datatype, *bsflag;
/* ------------------------------------------------------------------- */

    *iret = G_NORMAL;
/*
 *
 */
    if( usrSelect.group->prod[ item ].index == -1 ) {
        index = nwxtbl_sdtyp( usrSelect.group->prod[ item ].key );
	if( index >= 0 ) {
	    usrSelect.group->prod[ item ].index = index;
	}
    }

    srchInfo.idtyp = usrSelect.group->prod[ item ].index;
    datatype = nwxTable->dtyp_info[ srchInfo.idtyp ].datatyp;

    if( srchInfo.idtyp >= 0 ) {
        bsflag = nwxTable->dtyp_info[ srchInfo.idtyp ].bsflag;

	if( bsflag[ 0 ] == 'W' || ( bsflag[ 0 ] == 'O' &&
		strcmp( datatype, "TAFS_DEC" ) != 0 ) ) {
	    pdata_setTimeCovered( EVENT, &ier );
	    dslw_dttmSensitive( G_TRUE );
	}
	else {
	    pdata_setTimeCovered( SCHEDULED, &ier );
	    dslw_dttmSensitive( G_FALSE );
	}
    }

    dslw_apply( );

}
