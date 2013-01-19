#include "nwx_cmn.h"


#define DELMTR     "\n"

/************************************************************************
 * srcho.c                                                              *
 *                                                                      *
 * This module processes observed data for type "O".			*
 *                                                                      *
 * CONTENTS:                                                            *
 *      srcho_fosdGetrep()      finds the report for the requested stn	*
 *                              for the requested time.			*
 *									*
 ***********************************************************************/

/*=====================================================================*/
#define	EOL "\n"
void srcho_fosdGetrep ( srchinfo_t *srchinfo, int called_from,
			char *report, int *iret )
/************************************************************************
 * srcho_fosdGetrep                                                     *
 *                                                                      *
 * This routine will find the report for the requested location.        *
 *                                                                      *
 * srcho_fosdGetrep( srchinfo, called_from, report, iret )            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *srchinfo       srchinfo_t      Search info structure           *
 *      called_from     int     Indicate from where it is called        *
 *                                1 - from user click                   *
 *                                2 - from auto-update                  *
 *                                                                      *
 * Output parameters:                                                   *
 *      *report         char            Report                          *
 *      *iret           int             Return code                     *
 *                               0 -- successful                        *
 *                              -1 -- no data                           *
 *                              -6 -- last file in fore/backward        *
 *				-8 -- scandir failure			*
 *				-9 -- error opening file		*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 9/96	modified from srchw_fosdGetrep		*
 * D.W.Plummer/NCEP	10/96	Corrected for specials        		*
 * D.W.Plummer/NCEP	11/96	Change for new table for OBS		*
 * I. Durham/GSC	 5/98   changed call for underscore		*
 * D. Kidwell/NCEP      10/98   Changed output format for specials      *
 * A. Hardy/GSC		 3/99   Added priority parameter to sf_sstn     *
 * A. Hardy/GSC		 3/99   Removed ispri = 0			*
 * S. Jacobs/NCEP	 4/99	Changed cfdate to css_date; Changed	*
 *				to use 4 digit year for comparisons	*
 * D. Kidwell/NCEP	 4/99	Changed iyr < 20 to iyr <= 20           *
 * S. Danz/AWC		12/00	Updated call to dttm_cpy		*
 * S. Jacobs/NCEP	 8/01	Reordered output of specials		*
 * T. Piper/SAIC	 5/02	Added filnam to txtw_dttmSet		*
 * D. Kidwell/NCEP	 9/02	Added check for SFC_HRLY data type      *
 * T. Lee/SAIC		 2/03	TAFs display changes			*
 * D. Kidwell/NCEP	 4/03	Only display the latest TAF             *
 * R. Tian/SAIC         11/03   Added called_from arg                   *
 * M. Mainelli/TPC      11/03   Added check for SYN_DATA type		*
 * T. Piper/SAIC	01/04	Added check on nsfopn, added iret = -9	*
 * T. Piper/SAIC	01/04	Added check for -8 from dir_getflist	*
 * T. Piper/SAIC	02/04	Removed unused variable ihr		*
 * T. Piper/SAIC	04/05	Added SOUNDING DATA support	        *
 * D. Kidwell/NCEP	11/05	Changed str 1000 -> 3000, spstr 6 -> 30 *
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * J. Wu/SAIC		04/06	Added parameter in cst_wrap 		*
 * E. Safford/SAIC	12/07	make no_stid G_Boolean			*
 * M. James/Unidata	10/09	changed cst_ilst to allow for _ sep	*
 ***********************************************************************/
{
int		ii, last_flg, wrap_len = 74;
char            nofile_msg[80], stn[12], stid[12]="\0";
int		iflno, hour;
char		dattim[24], dattimx[24];
char		parts[4][5] = {"TXTA", "TXTB", "TXPB", "TXTC"};
int		istnm, lenstr, ihhmm, nrep;
float		slat, slon, selv;
char		stnm[6], str[3000]="\0";
char		blank[2]={' '}, spstr[30][256];
int		n, ispri, nsp, lensp;
char		sdate[11], cdate[11], tmpstr[20];
char		reptim[11], savtim[20], gtime[20];
char		*datatype, *ptr, *cp1, *cp2;
int		iarr[2], num, iyr, imn, idy, ier;
int		minutes, jarr[5], karr[5];
long		posn;
G_Boolean	no_stid;
static struct	date_time_info _startd_save;
static struct	date_time_info _endd_save;

/*---------------------------------------------------------------------*/

	last_flg = G_FALSE;
        datatype = nwxTable->dtyp_info[srchInfo.idtyp].datatyp;

/*
 * Scan the data directory for a list of valid files.
 */
        if ( (srchinfo->idtyp != _idtyp_save) ||
                (dttm_cmp(srchinfo->startd, _startd_save) != 0) ||
                (dttm_cmp(srchinfo->endd, _endd_save) != 0) )
        {
                _idtyp_save = srchinfo->idtyp;
                dttm_cpy(&_startd_save, srchinfo->startd);
                dttm_cpy(&_endd_save,   srchinfo->endd);

/*
 * For TAFs, subtract one day from the start time and
 * add one day to the end time to get files. 
 */
		if ( strcmp ( datatype, "TAFS_DEC" ) == 0 )  {
		    minutes = 1440;
		    srcho_timChange ( srchinfo, minutes, &ier );
		}

                dir_getflist( nwxTable->dtyp_info, srchinfo->idtyp, 
			srchinfo->startd, srchinfo->endd,
                        &srchinfo->dir_info, iret );
		if ( *iret == -8 ) return;
/*
 * Restore the start time for TAFs.
 */
		if  ( strcmp ( datatype, "TAFS_DEC" ) == 0 )  {
		    minutes = -1440;
		    srcho_timChange ( srchinfo, minutes, &ier );
		}

                if  ( *iret != 0 ) {
/*
 * no matching data file
 */
                    *iret = -1;
		    sprintf( nofile_msg, "No reports in the last %d hour(s).\n\n", 
						usrSelect.ndttm );
		    if ( called_from == 1 ) {
			pdata_setReportText( nofile_msg, &ier );
			txtw_dsplyReport( &ier );
/*		        XmTextSetString( textW, nofile_msg ); */
		    }
		    stnList.nrptstn = 0;
                    return;
                }
	}

	sprintf ( sdate, "%04d%02d%02d%02d", srchinfo->startd.year, 
		  srchinfo->startd.month, srchinfo->startd.day, 
		  srchinfo->startd.hour );

	last_flg = 0;
	report[0] = '\0';
	while ( !last_flg ) {

/*
 * Get the next data file.
 */
            dir_getnextf( &(srchinfo->dir_info), srchinfo->sflag,
                        &(srchinfo->file_info), iret);
            if ( *iret == 3 )
                        last_flg = 1;

            if ( (*iret == 0) || (*iret == 3) ) { /* new file */

/*
 * Open the file.
 */
		nsfopn( datatype, srchinfo->file_info.filnam, 
		   	&iflno, iret, strlen(datatype),
		    	strlen(srchinfo->file_info.filnam) );
		if ( *iret != 0 ) {
		    *iret = -9;
		    return;
		}
/*
 * Pull all the data from selected station and save in buffer
 */

		strcpy(tmpstr, (char*)strrchr(srchinfo->file_info.filnam,'/') );
		
                if  ( ( srchInfo.smethod == OBS )  &&
		    ( ( strcmp ( datatype, "SFC_HRLY" ) == 0 ) ||
		      ( strcmp ( datatype, "SND_DATA" ) == 0 ) ||
		      (	strcmp ( datatype, "SYN_DATA" ) == 0 ) ) ) {

	            cst_ilst ( &tmpstr[1], '_', IMISSD, 2, iarr, &num, &ier );
		    
                    idy = iarr[0] % 100;
		    imn = ( iarr[0] / 100 ) % 100;
		    iyr = iarr[0] / 10000;
		    if  ( iyr <= 20 )  iyr += 2000;
		    if  ( iyr < 100 )  iyr += 1900;

		    sprintf ( dattimx, "%04d%02d%02d", iyr, imn, idy );

		}
		else {
                    cst_ilst ( &tmpstr[1], '.', IMISSD, 2, iarr, &num, &ier );

		    idy = ( iarr[0] / 100 ) % 100;
		    imn = ( iarr[0] / 10000 ) % 100;
		    iyr = iarr[0] / 1000000;
		    if  ( iyr <= 20 )  iyr += 2000;
		    if  ( iyr < 100 )  iyr += 1900;

		    sprintf ( dattimx, "%04d%02d%02d",
			      iyr, imn, idy );

		}

		for ( hour = 23 ; hour >= 0 ; hour-- ) {

		    sprintf( cdate, "%s%02d", dattimx, hour );

		    sprintf( dattim, "%s/%02d00", &dattimx[2], hour );
		    if ( strcmp(datatype, "SND_DATA") == 0 ) {
			sn_stim (&iflno, dattim, iret, strlen(dattim) );
		    }
		    else {
	 	        sf_stim( &iflno, dattim, iret, strlen(dattim) );
		    }

		    if ( *iret == 0  &&  strcmp(sdate, cdate) < 0 ) {

			strcpy( stn, srchinfo->srchstr );
		        if ( strcmp(datatype, "SND_DATA") == 0 ) {
			    sn_sstn( &iflno, stn, stid, &istnm, &slat, 
				     &slon, &selv, iret, strlen(stn), 
				     strlen(stid) );
			}
                        else {
                            sf_sstn( &iflno, stn, stid, &istnm, &slat, 
			    	     &slon, &selv, &ispri, iret, 
				     strlen(stn), strlen(stid) );
			}
			if ( *iret == 0 ) {

/*
 *  Read specials
 */
			    if ( strcmp(datatype, "SND_DATA") != 0 ) {
				sf_rspc( &iflno, str, &lenstr, &ihhmm, 
					 &nrep, iret, sizeof(str) );
			        if ( lenstr > 0 && *iret == 0 && nrep > 0 ) {

/*
 * Reconstruct the specials. If the substring starts with the station
 * id, start a new special. Otherwise, concatenate it to the previous
 * special.
 */
				cst_ncpy ( stid, str, 4, &ier );
				nsp = -1;
				for ( n = 0 ; n < nrep ; n++ ) {
				    posn = n * 80;
				    if  ( strncmp ( &(str[posn]),
				    		    stid, 4 ) == 0 ) {
					nsp++;
				    	cst_ncpy ( spstr[nsp],
						   &(str[posn]),
						   80, &ier );
					cst_lstr ( spstr[nsp],
						   &lensp, &ier );
					spstr[nsp][lensp] = CHNULL;
				    }
				    else {
				    	if  ( nsp >= 0 )  {
					    cst_lstr ( spstr[nsp],
					    	       &lensp, &ier );
					    strncat ( spstr[nsp],
					    	     &(str[posn]), 80 );
					    spstr[nsp][lensp+80] =
					    			CHNULL;
					    cst_lstr ( spstr[nsp],
					    	       &lensp, &ier );
					    spstr[nsp][lensp] = CHNULL;
					}
				    }
				}

/*
 * Add each special to the report string, in reverse order.
 */
				for ( n = nsp; n >= 0; n-- )  {
			            strcat ( report, spstr[n] );
				    strcat ( report, "\n" );
				}

			        }	/* end if ( lenstr > 0 && *iret == 0 ... */
			    }	/* end if ( strcmp(datatype, "SND_DATA") != 0 )  */
/*
 *  Read string
 */

		            if ( strcmp(datatype, "SND_DATA") == 0 ) {
				no_stid = G_TRUE;
				for ( ii = 0; ii < 4; ii++ ) {
				    sn_rstr ( &iflno, parts[ii], str, &ihhmm,
				          &lenstr, iret, strlen(parts[ii]), 
					  sizeof(str) );
			            if ( *iret == 0 ) {
			                if ( no_stid ) {
					    strcat ( report, stn );
					    sprintf ( stnm, " - %d at %s\n", istnm, dattim );
					    strcat ( report, stnm );
					    no_stid = G_FALSE;
					}
					str[lenstr+1]='\0';
					strcat ( str, EOL );
					cst_wrap ( str, blank, &wrap_len, EOL, (char *)NULL, str, &ier);
					strcat ( report, str );
				    }
			        }
				if ( !no_stid ) strcat ( report, "\n" );
		            }
		            else {
			        sf_rstr( &iflno, str, &ihhmm, &lenstr, iret,
						sizeof(str) );
			        str[lenstr+1]='\0';
			        if ( lenstr > 0 && *iret == 0 )  {
				    if  ( strstr(str,"REPORT AT") == NULL ) {
				        if ( strrchr(str,'=') != NULL ){
				            posn = lenstr;
				            str[posn] = CHLF;
				            str[posn+1] = '\0';
			                    strcat( report, str );
				        }
				        else {
			                    str[lenstr] = CHLF;
			                    str[lenstr+1] = '\0';
			                    strcat( report, str );
				        }
                                        if ( strcmp ( datatype, "TAFS_DEC" ) == 0 ) {
                                            strcpy ( sdate, cdate );
                                        }
				    }
				    else {
				        ptr = str + 10;
				        strcpy ( gtime, ptr );
				        strcpy ( savtim, gtime );
				        cp1 = strtok ( gtime, "/");
				        cp2 = strtok ( NULL, "/" );
				        strcpy  ( reptim, "20" );
				        strcat  ( reptim, cp1 );
				        strncat ( reptim, cp2, 2 ); 

/*
 * If the report time is earlier or the same as the start time, subtract one 
 * hour from the report time to retrieve the data.
 */

				        if ( strcmp ( reptim, sdate ) < 0 ||
				             strcmp ( reptim, sdate ) == 0 ) {
					    ti_ctoi ( savtim, jarr, &ier, strlen(savtim) );
					    minutes = 60;
					    ti_subm ( jarr, &minutes, karr, &ier );
					    ti_itoc ( karr, savtim, &ier, strlen(savtim) );
					    strcpy  ( reptim, "20" );
					    cp1 = strtok ( savtim, "/");
					    cp2 = strtok ( NULL, "/" );
					    strcat  ( reptim, cp1 );
					    strncat ( reptim, cp2, 2 );
					    strcpy  ( sdate, reptim );
				        }
				    }  /* else  */
			        }  /*  end if ( lenstr > 0 && *iret == 0 )  */
			    }  /* else */
			}  /*  end if ( *iret == 0 ) */
		    }	/*  end if ( *iret == 0  &&  ...  ) */
		}  /*  end for ( hour  */
/*
 *  Close surface file
 */
		if ( strcmp(datatype, "SND_DATA") == 0 )
		    sn_clos( &iflno, iret );
		else
		    sf_clos( &iflno, iret );

                if (srchinfo->sflag == 0)
                    srchinfo->sflag = -1;

            }
            else
                break;

	}   /*  end while  */

	if ( strlen(report) == (size_t)0 ) {
		*iret = -1;
	}

/*
 * Remove the unprintable characters.
 */

        cst_unpr( report, report, iret );

/*
 * Display report date in the text window.
 */
	txtw_dttmSet(&tmpstr[1]); 
	txtw_dsplyDateTime( &ier );

/*
 * set the return code if it is the last file
 * in forward/backward
 */

        if ( last_flg )
                *iret = -6;

}

/*=====================================================================*/

void srcho_timChange ( srchinfo_t *srchinfo, int minutes, int *iret )
/************************************************************************
 * srcho_timChange							*
 *                                                                      *
 * This routine will recompute the start and end times for TAFs data.	*
 *                                                                      *
 * srcho_timChange( srchinfo, minutes, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *srchinfo       srchinfo_t      Search info structure           *
 *	 minutes	int		time in minutes			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                              					* 
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/SAIC		02/03						*
 * T. Lee/SAIC		08/03	Added 24 hrs to end time		*
 ***********************************************************************/
{
int		iarr[5], jarr[5], mins;

/*---------------------------------------------------------------------*/
/*
 * Subtract one day from start time.
 */
	iarr [0] = srchinfo -> startd.year;
	iarr [1] = srchinfo -> startd.month;
	iarr [2] = srchinfo -> startd.day;
	iarr [3] = srchinfo -> startd.hour;
	iarr [4] = 0;

	if ( minutes > 0 )  {
	    ti_subm ( iarr, &minutes, jarr, iret );
	}
	else {
	    mins = - minutes;
	    ti_addm ( iarr,  &mins, jarr, iret );
	}
	srchinfo -> startd.year  = jarr [0];
	srchinfo -> startd.month = jarr [1];
	srchinfo -> startd.day   = jarr [2];
	srchinfo -> startd.hour  = jarr [3];

/*
 * Add one day to end time.
 */
	iarr [0] = srchinfo -> endd.year;
	iarr [1] = srchinfo -> endd.month;
	iarr [2] = srchinfo -> endd.day;
	iarr [3] = srchinfo -> endd.hour;
	iarr [4] = 0;

	if ( minutes > 0 ) {
	    ti_addm ( iarr, &minutes, jarr, iret );
	}
	else {
	    mins = - minutes;
	    ti_subm ( iarr, &mins, jarr, iret );
	}
	srchinfo -> endd.year  = jarr [0];
	srchinfo -> endd.month = jarr [1];
	srchinfo -> endd.day   = jarr [2];
	srchinfo -> endd.hour  = jarr [3];
}
