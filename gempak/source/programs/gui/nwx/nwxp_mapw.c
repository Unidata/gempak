#include "nwx_cmn.h"


#ifndef FLT_MAX
#define FLT_MAX 3.40282347E+38
#endif	/* FLT_MAX */


int stnindex[LLSTFL]; 

/************************************************************************
 * nwxp_mapw.c								*
 *									*
 * This module manages the map station list (stnindex) and provides  	*
 * station handling utilities.                    		 	*
 *									*
 * CONTENTS:								*
 *	mapw_fndStn()		find the closest station marker		*
 *      mapw_getStation()	Get current value for a station index 	*
 *      mapw_pickStation()	pick a station given an x,y location  	*
 *   	mapw_setStation()	set station index value        		*
 *  	mapw_isSelected()	check if station is already selected	*
 *      mapw_rmappstn()		remove the appended stations		*
 *   	mapw_rmselstn() 	remove the selected stations		*
 *	mapw_updateMap()	process map menu callback		* 
 ***********************************************************************/

/*=====================================================================*/

int mapw_fndStn ( float *xin, float *yin )
/************************************************************************
 * mapw_fndStn								*
 *									*
 * This routine finds the closest station marker on the map window	*
 *									*
 * int _fnd_closestn ( xin, yin )					*
 *									*
 * Input parameters:							*
 *	*xin	float	input x-position				*
 *	*yin	float	input y-position				*
 *									*
 * Output parameters:							*
 *	_fnd_closestn	int	nearest station				*
 *									*
 **									*
 * L. Williams/EAI	10/95						*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * E. Safford/SAIC	12/07	rename func to mapw_fndStn		*
 ***********************************************************************/
{
int  found, nstn;
int  ii, ier=0;
float dist1, dist2;
float *rx, *ry;
double x, y;

/*---------------------------------------------------------------------*/

	if ( srchInfo.smethod == STANDARD || srchInfo.smethod == OBS ) {

		nstn = stnList.nstn;

		rx = (float *)malloc( sizeof(float) * (size_t)nstn);
		ry = (float *)malloc( sizeof(float) * (size_t)nstn);

/*
 * Transform from map coordinates to plot coordinates.
 */
		wgem_gtrans( sys_M, sys_D, &nstn, stnList.lat, stnList.lon,
			rx, ry, &ier );

	}
	else if ( srchInfo.smethod == WATCHWARN ) {

		nstn = stnList.nrptstn;

		if ( nstn == 0 ) {
			return ( -1 ) ;
		}
		else {

			rx = (float *)malloc( sizeof(float) * (size_t)nstn);
			ry = (float *)malloc( sizeof(float) * (size_t)nstn);

/*
 * Transform from map coordinates to plot coordinates.
 */
			wgem_gtrans( sys_M, sys_D, &nstn, stnList.rptstnlat, stnList.rptstnlon,
				rx, ry, &ier );
		}
	}

/*
 * Find the closest data point.
 */
	dist1  = FLT_MAX;
    for (ii=0; ii < nstn; ii++) {

	x =  (double)(rx[ii] - *xin);
	y =  (double)(ry[ii] - *yin);

	    dist2 = (float)(pow( x, 2.0 ) + pow( y, 2.0 ));

	    if	( dist2 < dist1 ) {
		dist1 = dist2;
	    found = ii;
	    }
	}

	free(rx);
	free(ry);
	return (found);
}

/*=====================================================================*/

int mapw_getStation ( int idx )
/************************************************************************
 * mapw_getStation  							*
 *									*
 * Get the current value for a given station index number.		*
 *									*
 * int mapw_getStation ( idx )                        			*
 *									*
 * Input parameters:							*
 *	idx		int	station index    			*
 * Output parameters:							*
 *			None						*
 * Return:								*
 *			int	value of requested station index	*
 **									*
 * E. Safford/SAIC	12/07	initial coding            		*
 ***********************************************************************/
{
    int		rc = -1;
/*---------------------------------------------------------------------*/

    if(  idx >= 0 || idx <= (LLSTFL - 1) ) {
        rc = stnindex[ idx ];
    }
    return( rc );
}

/*=====================================================================*/

void mapw_pickStation ( float xloc, float yloc, int *iret )
/************************************************************************
 * mapw_pickStation  							*
 *									*
 * Select a station/stations based on the xloc,yloc value.              *
 *									*
 * void mapw_pickStation ( xloc, yloc, iret )          			*
 *									*
 * Input parameters:							*
 *	xloc		float	x coordinate	    			*
 *	yloc		float	y coordinate	    			*
 *									*
 * Output parameters:							*
 * 	*iret		int	return code				*
 *				  0 = normal				*
 *				 -1 = too many stations already picked	*
 *				 -2 = not in station/watch selece mode  *
 *				 -3 = no station near xloc,yloc		*
 **									*
 * E. Safford/SAIC	12/07	initial coding            		*
 ***********************************************************************/
{
int		ifound, ier, stnValue;
int		ii, jj, kk, jcolr, imrk;
static int	pre_count = 0, too_many_append = 0;
int		cur_count = 0, tmpindex[LLSTFL];
float		tmplat[LLSTFL], tmplon[LLSTFL];
char		nodata_msg[80]="/0", tmpstr[20], date[ 30 ];
char		srchstr[LLSTFL][SRCHSTR_LEN];
char		report[ REPMAX ], fullReport[ REPMAX ];
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *  verify we are working with either stations or watchboxes, else
     *  exit.
     */
    if( plotData.mode != STNSELECT && plotData.mode != WATCHBOX ) {
    	*iret = -2;
        return;
    }

    /* 
     *  Verify we haven't aready maxed out the selected stations.
     */
    if( pre_count > LLSTFL - 2 ) {
        *iret = -1;
	return;
    }

    /*
     *  Set the search flag indicating it is new data.
     */
    srchInfo.sflag = 0;

    if( usrSelect.modeflg == REPLACE ) {

        /*
         *  Under replace mode, if there were selected stations, clear the
         *  selected station markers.
         */
        if( plotData.plt_mark.nstn ) {
            jcolr = RES_COL;
	    imrk  = SEL_MRK;
	    draw_stnmark( plotData.plt_mark.nstn, plotData.plt_mark.lat,
	    		  plotData.plt_mark.lon, jcolr, imrk, &ier );
	}

	/*
	 *  Under replace mode, redraw all station markers.
	 *
	 *  ES:  not sure why we're clearing & redrawing markers here.  
	 *  Look into this as time permits.
	 */

	jcolr = ALL_COL;
	imrk  = ALL_MRK;
	if( srchInfo.smethod == STANDARD || srchInfo.smethod == OBS ) {
            draw_stnmark( stnList.nstn, stnList.lat, stnList.lon, jcolr, imrk, &ier );
	}
	else if (srchInfo.smethod == WATCHWARN ) {
            draw_stnmark( stnList.nrptstn, stnList.rptstnlat, stnList.rptstnlon, 
	    		  jcolr, imrk, &ier );
	}
	
	/*
	 *  rest the previous count.
	 */
	pre_count = 0;
	too_many_append = 0;
    }

    /*
     *  prevent overflowing the print buffer.
     */
    if( too_many_append ) {
        *iret = -2;
	return;
    }

    /*
     *  Find the closest station to the press location.
     */
    ifound = mapw_fndStn( &xloc, &yloc );
    if( ifound == -1 ) {
        *iret = -3;
	return;
    }

    if( usrSelect.selct_by == STATION ) {

        /*
	 *  Set the selected lat,lon for a single station.
	 */
        if( srchInfo.smethod == STANDARD || srchInfo.smethod == OBS ) {
	    if( mapw_isSelected( pre_count, ifound ) ) {
	        *iret = -4;
	        return;
	    }
	    plotData.plt_mark.lat[ pre_count ] = stnList.lat[ ifound ];
	    plotData.plt_mark.lon[ pre_count ] = stnList.lon[ ifound ];
	    mapw_setStation( pre_count, ifound, &ier ); 
	}
	else if( srchInfo.smethod == WATCHWARN ) {
	    if( mapw_isSelected( pre_count, stnList.rptstn[ ifound ] ) ) {
	        *iret = -4;
		return;
            }
	    plotData.plt_mark.lat[ pre_count ] = stnList.rptstnlat[ ifound ];
	    plotData.plt_mark.lon[ pre_count ] = stnList.rptstnlon[ ifound ];
	    mapw_setStation( pre_count, stnList.rptstn[ ifound ], &ier ); 
	}

	cur_count = 1;
	plotData.plt_mark.nstn = pre_count + cur_count;
    }
    else if( usrSelect.selct_by == STATE ) {

        /*
	 *  Find all the stations in the selected state.
	 */
	if( srchInfo.smethod == STANDARD || srchInfo.smethod == OBS ) {
	    for( cur_count = 0, kk = 0; kk< stnList.nstn; kk++ ) {
	        if( strcmp( stnList.stateId[ ifound ], stnList.stateId[ kk ] ) == 0 &&
		    strcmp( stnList.counId[ ifound ], stnList.counId[ kk ] ) == 0 ) {

		    if( mapw_isSelected( pre_count, kk ) ) {
		        continue;
		    }

		    tmplat[ cur_count ] = stnList.lat[ kk ];
		    tmplon[ cur_count ] = stnList.lon[ kk ];
		    tmpindex[ cur_count ] = kk;
		    cur_count++;
		}
	    }

	    if( pre_count + cur_count > LLSTFL - 1 ) {
	        *iret = -2;
	        return;
	    }

	    for( jj= pre_count, kk=0; kk< cur_count; jj++, kk++ ) {
	        plotData.plt_mark.lat[ jj ] = tmplat[ kk ];
		plotData.plt_mark.lon[ jj ] = tmplon[ kk ];
		mapw_setStation( jj, tmpindex[ kk ], &ier );
	    }
	    plotData.plt_mark.nstn = pre_count + cur_count;
	}
	else if( srchInfo.smethod == WATCHWARN ) {
            for( cur_count=0, kk=0; kk < stnList.nrptstn; kk++ ) {
	        if( strcmp( stnList.stateId[ stnList.rptstn[ ifound ]], 
		            stnList.stateId[ stnList.rptstn[ kk ]] ) == 0 &&
	            strcmp( stnList.counId[ stnList.rptstn[ ifound ]],
		            stnList.counId[ stnList.rptstn[ kk ]] ) == 0 ) {
	            if( mapw_isSelected( pre_count, stnList.rptstn[ kk ] )) {
		        continue;
	            }

		    tmplat[ cur_count ] = stnList.rptstnlat[ kk ];
		    tmplon[ cur_count ] = stnList.rptstnlon[ kk ];
		    tmpindex[ cur_count ] = stnList.rptstn[ kk ];
		    cur_count++;
                }
	    }
            if( pre_count + cur_count > LLSTFL - 1 ) {
	        *iret = -2;
		return;
	    }

	    for( jj=pre_count, kk=0; kk<cur_count; jj++, kk++ ) {
	        plotData.plt_mark.lat[ jj ] = tmplat[ kk ];
	        plotData.plt_mark.lon[ jj ] = tmplon[ kk ];
		mapw_setStation( jj, tmpindex[ kk ], &ier );
	    }

	    plotData.plt_mark.nstn = pre_count + cur_count;
	}
    }  /* selct_by == STATE */

    /*
     *  Plot the selected station markers.
     */
    jcolr = SEL_COL;
    imrk  = SEL_MRK;

    draw_stnmark( cur_count, &plotData.plt_mark.lat[ pre_count ],
    		  &plotData.plt_mark.lon[ pre_count ], jcolr, imrk, &ier );

    if( plotData.mode != WATCHBOX ) {
        for( ii=pre_count; ii < plotData.plt_mark.nstn; ii++ ) {
	    stnValue = mapw_getStation( ii );
	    strcpy( srchstr[ ii-pre_count ], stnList.srchstr[ stnValue ] );
	}

	qsort( srchstr, plotData.plt_mark.nstn-pre_count, SRCHSTR_LEN,
	       (int(*)(const void*, const void*))strcmp);
  	for( ii=pre_count; ii< plotData.plt_mark.nstn; ii++ ) {
	    srchInfo.sflag = 0;

            /*
	     *  Search for the most recent report.
	     */
	    strcpy( srchInfo.srchstr, srchstr[ ii-pre_count ] );
	    fosd_getdata( nwxTable->dtyp_info, &(srchInfo), 1, report, &ier );
	    if( ier < -7 ) {
	        *iret = -5;
		return;
	    }
	    
	    /*
	     *  If the report is empty.
	     */
	    if( !report[0] ) {
	        if( ii == 0 ) {
		    strcpy( date, " " );
		    pdata_setDateTime( date, &ier );
		    txtw_dsplyDateTime( &ier );

		    if( usrSelect.selct_by == STATION ) {
		        txtw_setPrevNext( G_FALSE, G_FALSE );
		    }
		}

		sprintf( nodata_msg, 
		    "No data available for %s in the last %d hour(s).\n",
		    		srchInfo.srchstr, usrSelect.ndttm );
		strcpy( report, nodata_msg );
	    }

	    /*
	     *  Display the text report in the text window.
	     */
	    if( ii == 0 ) {
	        strcpy( fullReport, report );
		strcpy( printText, report );
	    }
	    else {
	        if( strlen( printText ) + strlen( report ) > (size_t)( PRNMAX -1 )) {
		    sprintf( report, "Too many appended reports\n" );
		    strcat( fullReport, report );
		}
		else {
		    strcat( fullReport, report );
		    strcat( printText, report );
		}
            }
	}
	if( usrSelect.selct_by == STATE ) {
	    strcat( fullReport, "----------\n" );
	    strcat( printText, "----------\n" );
	}
	pdata_setReportText( fullReport, &ier );
	txtw_dsplyReport( &ier );

	/*
	 *  Activate previous/next buttons if selected by station.
	 */
	if( srchInfo.smethod != OBS && usrSelect.selct_by == STATION &&
	            usrSelect.modeflg == REPLACE && 
	            strncmp( reportText, nodata_msg, 7 ) != 0 ) {
            txtw_setPrevNext( G_TRUE, G_TRUE );
	}
    }
    else {
	strcpy( tmpstr, (char*)strrchr( _rstd[ ifound ].fname, '/' ) );
	txtw_dttmSet( &tmpstr[1] );
	txtw_dsplyDateTime( &ier );

	pdata_setReportText( wtchText[ifound], &ier );
	txtw_dsplyReport( &ier );
    }

    pre_count += cur_count;
}

/*=====================================================================*/

void mapw_setStation ( int idx, int value, int *iret )
/************************************************************************
 * mapw_setStation  							*
 *									*
 * Set the current value for a given station index number.		*
 *									*
 * void mapw_setStation ( idx, value, iret )           			*
 *									*
 * Input parameters:							*
 *	idx		int	station index    			*
 *	value		int	station value    			*
 *									*
 * Output parameters:							*
 *			int	return code				*
 *				  0 = normal				*
 *				 -1 = stnIndex is out of range		*
 *				 -2 = value is out of range		*
 * Return:								*
 *			int	value of requested station index	*
 **									*
 * E. Safford/SAIC	12/07	initial coding            		*
 ***********************************************************************/
{
    *iret = G_NORMAL;

    if(  idx < 0 || idx > LLSTFL - 1 ) {
        *iret = -1;
    }
    else if( value < -1 || value > LLSTFL - 1 ) {
     	*iret = -2; 
    }

    if( *iret >= G_NORMAL ) {
        stnindex[ idx ] = value;
    }
}

/*=====================================================================*/

int mapw_isSelected ( int numsel, int station )
/************************************************************************
 * mapw_isSelected  							*
 *									*
 * This routine checks if the station is already selected		*
 *									*
 * int mapw_isSelected ( numsel, station )				*
 *									*
 * Input parameters:							*
 *	numsel		int	number of selected stations		*
 *	station		int	current selected station		*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * R. Tian/SAIC		 4/03						*
 * E. Safford/SAIC	12/07	renamed to mapw_isSelected		*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    for ( ii = 0; ii < numsel; ii++ ) {
    	if ( stnindex[ii] == station ) {
	    return 1;
	 }
    }
    return 0;
}

/*=====================================================================*/

void mapw_rmappstn ( void )  
/************************************************************************
 * mapw_rmappstn							*
 *									*
 * This routine removes the appended stations.				*
 *									*
 * void mapw_rmappstn ( )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * R. Tian/SAIC		 7/03						*
 ***********************************************************************/
{
    int		ii, ier; 
/*---------------------------------------------------------------------*/

    for ( ii = 0; ii < LLSTFL; ii++ ) { 
	mapw_setStation( ii, -1, &ier );
    }
}

/*=====================================================================*/

void mapw_rmselstn ( void )
/************************************************************************
 * mapw_rmselstn							*
 *									*
 * This function removes the selected stations. 			*
 *									*
 * void mapw_rmselstn()							*
 *									*
 * Input Parameters:							*
 *   None								*
 *									*
 * Output Parameters:							*
 *   None								*
 *									*
 **									*
 * Log: 								*
 * L. Williams		05/96						*
 * C. Lin		05/96	add plot mode checking			*
 ***********************************************************************/
{
    if ( plotData.mode == STNSELECT)
	plotData.plt_mark.nstn = 0;
}

/*=====================================================================*/

void mapw_updateMap ( int mapIndex, int *iret )
/************************************************************************
 * mapw_updateMap  							*
 *									*
 * Update the internal map data in reponse to a change in the selected  *
 * map area menu.							*
 *									*
 * int mapw_updateMap ( mapIndex, iret )				*
 *									*
 * Input parameters:							*
 *	mapIndex	int	index number of selected map		*
 * Output parameters:							*
 *	iret		int	return code				*
 *				   0 = G_NORMAL				*
 **									*
 * E. Safford/SAIC	12/07	moved code from mapw_mapCb()		*
 ***********************************************************************/
{
    *iret = G_NORMAL;

/*
 *  Set the global variable for the map menu selection.
 */
    usrSelect.mapindx = mapIndex;

/*
 *  draw the map.
 */
    usrSelect.zoomflg = NO_ZOOM;
    fosd_plot( 1 );
}
