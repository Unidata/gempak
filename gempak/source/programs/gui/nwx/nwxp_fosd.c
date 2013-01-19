#include "nwx_cmn.h"


/************************************************************************
 * nwxp_fosd.c                                                          *
 *                                                                      *
 * This module processes FOS (Family Of Services) data.          	*
 *                                                                      *
 * CONTENTS:                                                            *
 *      fosd_txtrd()     read, process, display FOS data in text window.*
 *      fosd_decode()    decode FOS data for plotting.   		*
 *      fosd_plot()      plot FOS data.   				*
 *	fosd_getdata()	 get FOS data					*
 *	fosd_wbox()	 get, decode and plot current watch boxes	*
 ***********************************************************************/

/*=====================================================================*/

int fosd_txtrd ( int called_from ) 
/************************************************************************
 * fosd_txtrd								*
 *									*
 * This routine will read and display the text data.			*
 * Only single station selection will call this routine.		*
 *									*
 * int fosd_txtrd ( called_form ) 					*
 *									*
 * Input parameters:							*
 *      called_from     int     Indicate from where it is called        *
 *                                1 - from user click                   *
 *                                2 - from auto-update                  *
 *									*
 * Return parameters:							*
 * fosd_txtrd		int	-6 -- same as the previous displayed    *
 *				-3 -- last file in fore/back direction  *
 **									*
 * Log:									*
 * C. Lin/EAI		 10/95                   			*
 * D.W.Plummer/NCEP	  9/96	adjusted nodata_msg to include hours	*
 * S. Jacobs/NCEP	 3/99	Removed & from var in fosd_getdata call	*
 * M. Li/GSC		 2/00	nodata_msg[50] -> nodata_msg[80]	*
 * T. Piper/SAIC	 5/02	removed unneeded call to txtw_dttmSet	*
 * T. Piper/SAIC	 6/02	added iret = -6 to handle iret = -1 case*
 * R. Tian/SAIC		11/03	Added called_from arg			*
 * T. Piper/SAIC	01/04	Added check for < -7 from fosd_getdata	*
 * E. Safford/SAIC	12/07	use txtw_setPrevNext()			*
 ***********************************************************************/
{
int	       iret, ier;
char           nodata_msg[80], report[ REPMAX ], time[30];

/*---------------------------------------------------------------------*/


    if ( (srchInfo.sflag == 0) && (plotData.mode != STNSELECT) ) {
        if ( nwxTable->dtyp_info[srchInfo.idtyp].bsflag[0] == 'Z' ) {
            strcpy(srchInfo.srchstr, stnList.bulstr);
	}
        else
            strcpy(srchInfo.srchstr, stnList.srchstr[0]);
    }

    fosd_getdata( (nwxTable->dtyp_info), &srchInfo, called_from, report, &iret);
    if ( iret < -7 ) {
        return(iret);
    }
    else if ( (iret == 0 || iret == -6 ) && (report[0]) ) {
        printText[0] = '\0';
    }
    else {
        iret = -6;
        if ( !usrSelect.prvnxt ) {
            strcpy( time, " " );
            pdata_setDateTime( time, &ier );
            txtw_dsplyDateTime( &ier );
            sprintf(nodata_msg,
		    "No data available for %s in the last %d hour(s).\n",
                            srchInfo.srchstr, usrSelect.ndttm );
            if ( called_from == 1 ) {
                strcpy( report, nodata_msg );
      	    }
	    txtw_setPrevNext( G_FALSE, G_FALSE ); 
	}
    }

    /*
     *  Store and display the text report.
     */
    pdata_setReportText( report, &ier );
    txtw_dsplyReport( &ier );

    if ( (iret == -6) && report[0] ) {
        iret = -3; 
    }

    return( iret );
}


/*=====================================================================*/

void fosd_decode ( void ) 
/************************************************************************
 * fosd_decode								*
 *									*
 * This routine will decode the FOS data.				*
 *									*
 * fosd_decode ( )  							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 **									*
 * Log:									*
 * C. Lin/EAI		10/95                   			*
 * S. Jacobs/NCEP	 5/02	Added default case			*
 ***********************************************************************/
{
char    *datatype, report[ REPMAX ];
int	ier;

/*---------------------------------------------------------------------*/

    datatype = nwxTable->dtyp_info[srchInfo.idtyp].datatyp;
    pdata_getReportText( report, &ier );

    switch ( plotData.mode ) {

        case VALUE:

            if(strcmp(datatype, "UVI" ) == 0)
                uvi_decode( report, &stnList, &plotData );

            if (strcmp(datatype, "72_PRF_PRG" ) == 0)
                prf_decode( report, &stnList, &plotData );

	     break;

        case GRAPHIC:
	    
            if (strcmp(datatype, "QPF_DISC") == 0 ) 
                      qpf_decode( report, &plotData );

	    break;

	default:
    	    break;
        }
}

/*=====================================================================*/

void fosd_plot ( int called_from  )
/************************************************************************
 * fosd_plot								*
 *									*
 * This routine will plot the FOS data.					*
 *									*
 * fosd_plot ( called_from )  						*
 *									*
 * Input parameters:							*
 *      called_from     int     Indicate from where it is called        *
 *                                1 - from user click                   *
 *                                2 - from auto-update                  *
 *									*
 * Output parameters:							*
 **									*
 * Log:									*
 * C. Lin/EAI		10/95                   			*
 * S. Jacobs/NCEP	 8/98	Added call to fosd_wbox			*
 * S. Jacobs/NCEP	 5/02	Added default case			*
 * R. Tian/SAIC		 6/03	Added plot of selected station markers	*
 * R. Tian/SAIC		11/03	Added called_from arg			*
 ***********************************************************************/
{
int   jcolr, imrk, iret;

/*---------------------------------------------------------------------*/
/*
 * Draw the map.
 */
        draw_map( usrSelect.mapindx, nwxTable->map_info,
                                (int)usrSelect.zoomflg, &mapBnd, &iret );

        switch ( plotData.mode ) {

            case STNSELECT: /* plot all station markers */

                        jcolr = ALL_COL;
                        imrk  = ALL_MRK;
			if ( srchInfo.smethod == STANDARD || 
			     srchInfo.smethod == OBS ) {
                        	draw_stnmark ( stnList.nstn,
					       stnList.lat,
					       stnList.lon,
                               		       jcolr, imrk, &iret );
			}
			else if ( srchInfo.smethod == WATCHWARN ) {
                        	draw_stnmark ( stnList.nrptstn,
					       stnList.rptstnlat,
					       stnList.rptstnlon,
                               		       jcolr, imrk, &iret );
			}

/*
 * Plot selected station markers.
 */
            		jcolr = SEL_COL;
            		imrk  = SEL_MRK;
                    	draw_stnmark( plotData.plt_mark.nstn,
                                      plotData.plt_mark.lat,
                                      plotData.plt_mark.lon,
                                      jcolr, imrk, &iret);

                        break;

            case VALUE: /* plot the values */

                        draw_value ( &(plotData.plt_mark) );
                        break;

            case GRAPHIC: /* plot the contour line */

                        draw_cntr ( &(plotData.plt_cnt) );
                        break;

            case WATCHBOX: /* plot the watch box */

                        fosd_wbox ( called_from );
                        break;

	    default:
	    		break;

        }

}

/*=====================================================================*/

void fosd_getdata ( struct datatype_list *dt_info, srchinfo_t *srch_info, 
		    int called_from, char *report, int *iret )

/************************************************************************
 * fosd_getdata								*
 *									*
 * This routine get the FOS data.					*
 *									*
 * fosd_getdata ( dt_info, srch_info, called_from, report, iret ) 	*
 *									*
 * Input parameters:							*
 *	*dt_info	struct datatype_list	Data type structure	*
 *	*srch_info	srchinfo_t		Search info structure	*
 *      called_from     int     Indicate from where it is called        *
 *                                1 - from user click                   *
 *                                2 - from auto-update                  *
 *									*
 * Output parameters:							*
 *	*report		char		Report				*
 *	*iret		int		Return code			*
 *					 0 -- successful		*
 *					-1 -- no data			*
 *					-6 -- last file in 		*
 *						fore/backward		*
 *					-8 -- scandir failure		*
 *					-9 -- error opening file	*
 **									*
 * Log:									*
 * L. Williams/EAI	 6/96	Modified from srchb_fosdGetrep		*
 * L. Williams/EAI	 7/96	change searching method for "S" type	*
 * D.W.Plummer/NCEP	 9/96	Added "O" observed data type processing	*
 * R. Tian/SAIC		11/03	Added called_from arg			*
 * T. Piper/SAIC	01/04	Added iret = -8 & -9 comment		*
 * T. Piper/SAIC	01/04	Added nwxerr				*
 * E. Safford/SAIc	12/07	rename nwxerr to err_showError()	*
 ***********************************************************************/
{
int  ier;

/*---------------------------------------------------------------------*/

	if( srchInfo.smethod == STANDARD ) {
/*
 * check if search is starting over
 */
  	   if( ( srch_info->sflag == 0 ) || 
	       (nwxTable->dtyp_info [srch_info->idtyp].bsflag[0] == 'S' ) ) {
/*
 * Initialize the report_infoSTD structure
 */
	      srchb_repInit();

/*
 * find the first report
 */
	      srchb_fosdGetrep( dt_info, srch_info, report, iret ) ;

	   }
	   else {

/*
 * search the current file for the next report
 */
	      ier=0;
	      srchb_fosdGetnxt( srch_info, report, &ier );
	      switch( ier ) {

		  case -1:         /* report not found - search next file */
		     *iret = 0;
		     srchb_fosdGetrep( dt_info, srch_info, report, iret ) ;
		     break;

		  case -2:         /* last file in fore/backward  */
		     *iret = -6;
		     break;

		  case -9:         /* no data */
		     *iret = -9;
		     break;

		  default:         /* report found */
		     *iret = 0;
		     break;
	     }
	   }
	}
	else if( srchInfo.smethod == WATCHWARN ) {
	    srchw_fosdGetrep( srch_info, report, iret ) ;
	}
	else if( srchInfo.smethod == OBS ) {
	    srcho_fosdGetrep( srch_info, called_from, report, iret ) ;
	}
        if ( *iret < -7 ) {
	    err_showError( *iret );
        }

}

/*=====================================================================*/

void fosd_wbox ( int called_from ) 
/************************************************************************
 * fosd_wbox								*
 *									*
 * This routine will get, decode and plot the WATCHBOX data.		*
 *									*
 * fosd_wbox ( called_from )  						*
 *									*
 * Input parameters:							*
 *      called_from     int     Indicate from where it is called        *
 *                                1 - from user click                   *
 *                                2 - from auto-update                  *
 *									*
 * Output parameters:							*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/98						*
 * S. Jacobs/NCEP	 3/99	Removed & from var in fosd_getdata call	*
 * S. Chiswell/Unidata	 5/00	Added init for search string		*
 * S. Jacobs/NCEP	 2/01	Reorder the watch reports in place	*
 * T. Piper/SAIC	 7/02	Fixed search for two reports in one file*
 * T. Piper/SAIC	 7/02	Sorted fname to get proper date/time 	*
 * R. Tian/SAIC		11/03	Added called_from arg			*
 * T. Piper/SAIC	01/04	Added check for < -7 from fosd_getdata  *
 ***********************************************************************/
{

	int		ii, jj, nw, iret;

	char		tfname[133], treprt[REPMAX];

/*---------------------------------------------------------------------*/

	iret = 0;

	srchInfo.sflag = 0;
	strcpy(srchInfo.srchstr, stnList.srchstr[0]);

	nw = 0;

/*
 * Get all of the watches from the data files.
 */
  	while ( iret == 0 )  {
/*
 *  Note to self:  reportText here is a problem.  Should not access the global var.
 */
	    fosd_getdata ( (nwxTable->dtyp_info), &srchInfo, called_from,
			   reportText, &iret);
	    if ( iret < -7 ) {
		return;
	    }
    	    else if  ( iret == 0 )  {
		strcpy ( wtchText[nw], reportText );

		srchInfo.sflag = -1;
		nw++;
	    }
	}

/*
 * Reorder the array of report strings.
 */
  	for ( ii = 0, jj = nw-1; ii < nw/2; ii++, jj-- )  {
	    strcpy ( tfname, _rstd[ii].fname );
	    strcpy ( treprt, wtchText[ii] );
	    strcpy ( _rstd[ii].fname, _rstd[jj].fname );
	    strcpy ( wtchText[ii], wtchText[jj] );
	    strcpy ( _rstd[jj].fname, tfname );
	    strcpy ( wtchText[jj], treprt );
	}

/*
 * Decode each watch.
 */
	for ( ii = 0; ii < nw; ii++ )  {
	    wbox_decode ( wtchText[ii], ii, &plotData );
	}

	draw_wbox( &(plotData.plt_wbox), called_from );

}

