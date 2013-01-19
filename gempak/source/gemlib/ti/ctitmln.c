#include "geminc.h"
#include "gemprm.h"

void cti_tmln ( char **timin, int nin, int mrange, int intrvl,
                int idir, int iflag, int iauto, char *basetm,
                char *endtim, int *idelrt, char **timout,
                int *ntime, int *iret )
/************************************************************************
 * cti_tmln                                                             *
 *                                                                      *
 * This subroutine returns the time steps for time line based on input  *
 * time and interval.  If the interval is 0 or negative, all time       *
 * steps will be returned.                                              *
 *                                                                      *
 * cti_tmln ( timin, nin, mrange, intrvl, idir, iflag, iauto, basetm,   *
 *              endtim, idelrt, timout, ntime, iret )                   *
 *                                                                      *
 * Input parameters:							*
 *	**timin		char	Date/time				*
 *	nin		int	Number of times				*
 *	mrange		int	Time range in minutes			*
 *	intrvl		int	Time interval in minutes		*
 *	idir		int	Time line direction			*
 *                                       -1: backward/image data        *
 *                                        0: backward                   *
 *                                        1: forward                    *
 *	iflag		int	Reference time flag			*
 *                                       -1: for data loading           *
 *                                        0: no reference time          *
 *                                        1: reference time used        *
 *	iauto		int	Auto update flag			*
 *                                        0: no auto update             *
 *                                        1: auto update                *
 * Input and Output parameters:                                         *
 *	*basetm		char	Base time				*
 *	*endtim		char	End time, could be ref. time		*
 *					or system time			*
 *	*idelrt		int	Delta reference time in minutes		*
 *                                                                      *
 * Output parameters:                                                   *
 *	**timout	char	Date/time                       	*
 *	*ntime		int	Number of time                  	*
 *	*iret		int	Return code                     	*
 *                                        0 = normal return             *
 *                                       -5 = no time in data set       *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	09/07	Re-wrote ti_tmln in C.			*
 * T. Piper/SAIC	09/07	Added '\0' after call to ti_itoc	*
 * T. Piper/SAIC	12/07	Added check on ntime			*
 ***********************************************************************/
{
    int     idt, ier, ii, irange, itarr[5], jj, jrange, jtarr[5],
	    nmarks, nmin;
    char    tms[DTTMS_SIZE];
    Boolean done;
/*----------------------------------------------------------------------*/

    if ( nin == 0 ) {
	timout[0] = '\0';
	*ntime = 0;
	*iret = -5;
	return;
    }
    else if ( nin == 1 ) {
	if ( timout[0] != timin[0] ) strcpy(timout[0], timin[0]);
	*ntime = 1;
	return;
    }
    *iret = 0;
/*
 *  Get right sorting order.  Observations and analysis data are in
 *  descending order.  Forecast data are in ascending order.
 */
    ti_diff(timin[1], timin[0], &idt, iret, strlen(timin[1]), strlen(timin[0]));
    if ( idt == 0 ) {
	if ( timout[0] != timin[0] ) strcpy(timout[0], timin[0]);
	*ntime = 1;
	return;
    }
    else if ( idt > 0 ) {

/*
 *  If data time is in ascending order, resequence to
 *  descending order for obs., image, and analysis data.
 */
	if ( idir <= 0 ) {
	    cti_rseq(nin, timin, timin );
	}
    }
    else {

/*
 *  For forecast grid, the data is in ascending order.
 */
	if ( idir == 1 ) {
	    cti_rseq(nin, timin, timin );
	}
	idt = -idt;
    }

/*
 *  If iflag less than zero, return all data for data loading
 *  and time matching.
 */
    if ( iflag < 0 ) {
	for ( ii=0; ii<nin; ii++) {
	    if ( timout[ii] != timin[ii] ) strcpy(timout[ii], timin[ii]);
	}
	*ntime = nin;
	return;
    }

/*
 *  If delta reference time is not blank, get new endtime and set
 *  iflag to 1 to facilitate calculation.
 */
    if ( *idelrt > 0 && iflag == 0 ) {
	ti_ctoi(timin[0], itarr, &ier, strlen(timin[0]));
	if ( idir <= 0 ) {
	    ti_subm(itarr, idelrt, jtarr, &ier);
	}
	else {
	    ti_addm(itarr, idelrt, jtarr, &ier);
	}
	ti_itoc(jtarr, (char*)endtim, &ier, strlen(endtim));
	endtim[DTTMS_SIZE-1] = '\0';
	iflag = 1;
    }

/*
 *  If reference time is used, use endtime to compute data range.
 *  If time range (MRANGE) is greater than data range, the latter
 *  will be used for time range.
 */
    if ( iflag == 1 ) {
	if ( idir <= 0 ) {
	    ti_diff((char*)endtim, timin[nin-1], &irange, &ier,
				strlen(endtim), strlen(timin[nin-1]));
	    if ( irange < 0 ) {
		return;
	    }
	    else {
		ti_diff(timin[0], (char*)endtim, idelrt, &ier,
				strlen(timin[0]), strlen(endtim));
	    }
	}
	else {
	    ti_diff(timin[nin-1], (char*)endtim, &irange, &ier,
				strlen(timin[nin-1]), strlen(endtim));

	    if ( irange < 0 ) {
		timout[0] = '\0';
		*ntime = 0;
		*iret = -5;
		return;
	    }
	    else {
		ti_diff((char*)endtim, timin[0], idelrt, &ier,
				strlen(endtim), strlen(timin[0]));
	    }
	}
    }
    else {
	ti_diff(timin[nin-1], timin[0], &irange, iret,
				strlen(timin[nin-1]), strlen(timin[0]));
    }

    if ( ( mrange > G_ABS(irange) ) || ( mrange <= 0 ) )
	mrange = G_ABS(irange);
    if ( intrvl > 0 ) nmarks = mrange / intrvl + 1;
    *ntime = 0;
    done = FALSE;


    if ( intrvl <= 0 ) {
	ii = 0;
	if ( iauto > 0 ) {

/*
 *  Auto-update: ON (image data only)
 *  Intervals: OFF
 *  Reference time: OFF
 */
	    while ( ! done ) {
	        ti_diff(timin[0], timin[ii], &jrange, &ier,
				strlen(timin[0]), strlen(timin[ii]));
		jrange = G_ABS(jrange);
		if (  ( ier == 0 ) && ( jrange <= mrange ) ) {
		    (*ntime)++;
		    if ( timout[*ntime-1] != timin[ii] )
			strcpy(timout[*ntime-1], timin[ii]);
	        }
	        else {
		    done = TRUE;
		}
		ii++;
		if ( ii == nin ) done = TRUE;
	    }
	}
	else {

/*
 *  Auto-update: OFF
 *  Intervals: OFF
 *  Reference time: ON
 */
	    if ( iflag == 1 ) {
		while ( ! done ) {
		    if ( idir >= 1 ) {
		        ti_diff(timin[ii], (char*)endtim, &jrange, &ier,
				strlen(timin[ii]), strlen(endtim));
		    }
		    else {
		        ti_diff((char*)endtim, timin[ii], &jrange, &ier,
				strlen(endtim), strlen(timin[ii]));
		    }
		    if ( ( ier != 0 ) || ( jrange > mrange) ) {
			done = TRUE;
		    }
		    else {
			if ( jrange >= 0 ) {
			    (*ntime)++;
			    if ( timout[*ntime-1] != timin[ii] )
				strcpy(timout[*ntime-1], timin[ii]);
			}
		    }
		    ii++;
		    if ( ii == nin ) done = TRUE;
		}
	    }
	    else {

/*
 *  Auto-update: OFF
 *  Intervals: OFF
 *  Reference time: OFF
 */
		while ( ! done ) {
		    ti_diff(timin[0], timin[ii], &jrange, &ier,
				strlen(timin[0]), strlen(timin[ii]));
		    jrange = G_ABS(jrange);
		    if ( ( ier == 0 ) && ( jrange <= mrange ) ) {
			(*ntime)++;
			if ( timout[*ntime-1] != timin[ii] )
				strcpy(timout[*ntime-1], timin[ii]);
		    }
		    else {
		        done = TRUE;
		    }
		    ii++;
		    if ( ii == nin ) done = TRUE;
	        }
	    }
	}
    }
    else {

/*  Auto-update: ON/OFF
 *  Intervals: ON
 *  Reference time: OFF
 *  (for image data only: if auto update is on or get correct time line
 *   when users turn the auto-update button off then turn it back on).
 */
	if ( ( idir < 0 ) && ( iflag == 0 ) ) {
	    if ( strncmp(basetm, " ", 1) == 0 ) {
		ti_ctoi((char*)basetm, itarr, &ier, strlen(basetm) );
		ti_diff(timin[0], (char*)basetm, &nmin, &ier,
				strlen(timin[0]), strlen(basetm));
		if ( nmin >= intrvl ) {
		    nmin = ( nmin / intrvl ) * intrvl;
		    ti_addm(itarr, &nmin, jtarr, &ier);
		    ti_itoc(jtarr, (char*)basetm, &ier, strlen(basetm));
		    basetm[DTTMS_SIZE-1] = '\0';
		    ti_ctoi((char*)basetm, itarr, &ier, strlen(basetm));
		}
		strcpy(tms, basetm);
	    }
	    else {
		strcpy(tms, timin[0]);
		ti_ctoi(tms, itarr, &ier, strlen(tms));
	    }
	    while ( ! done ) {
		(*ntime)++;
		strcpy(timout[*ntime-1], tms);
		ti_subm(itarr, &intrvl, jtarr, &ier);
		ti_itoc(jtarr, tms, &ier, strlen(tms));
		tms[DTTMS_SIZE-1] = '\0';
		for ( jj=0; jj<5; jj++) {
		    itarr[jj] = jtarr[jj];
		}
		nmarks--;
		if ( nmarks <= 0 || *ntime == nin ) done = TRUE;
	    }
	}
	else {

/*
 *  Auto-update: OFF
 *  Intervals: ON
 *  Reference time: ON
 */
	    if ( iflag == 1 ) 
		strcpy(tms, endtim);
	    else {

/*  Auto-update: OFF
 *  Intervals: ON
 *  Reference time: OFF
 */
		strcpy(tms, timin[0]);
	    }
	    ti_ctoi(tms, itarr, &ier, strlen(tms));
	    while ( ! done ) {
		(*ntime)++;
		strcpy(timout[*ntime-1], tms);
		if ( idir <= 0 )
		    ti_subm(itarr, &intrvl, jtarr, &ier);
		else 
		    ti_addm(itarr, &intrvl, jtarr, &ier);
		ti_itoc(jtarr, tms, &ier, strlen(tms));
		tms[DTTMS_SIZE-1] = '\0';
		nmarks--;
		if ( nmarks <= 0 || *ntime == nin ) {
		    done = TRUE;
		}
		else {
		    for(jj=0;jj<5;jj++) {
			itarr[jj] = jtarr[jj];
		    }
	        }
	    }
	} 
    }
}
