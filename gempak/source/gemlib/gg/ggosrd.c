#include "geminc.h"
#include "gemprm.h"

/* Useful parameters */
#define LENTIM	   4
#define LENARR	  36
#define LENINT	 ( ( 9 * LENARR + LENTIM ) / 2 )
#define LENREAD	 ( LENINT * 4 )

#define LENARR_HI 76
#define LENINT_HI ( ( 9 * LENARR_HI + LENTIM ) / 2 )
#define LENREAD_HI  ( LENINT_HI * 4 )

#define LENPLT	5000
#define NUMPLT	  20
#define NLIMIT	  ( LENPLT - LENARR - 1 )
#define NLIMIT_HI ( LENPLT - LENARR_HI - 1 )

#define RNFAIL	0
#define	RNFCLR	1
#define	CIRCLE	2

#define ISRAIN  3

/* Structures for reading the data from the file */
typedef struct osdata {
	short		itm[LENTIM];
	short		lat[LENARR];
	unsigned short	lon[LENARR];
	unsigned short	iql[LENARR];
	short		isp[LENARR];
	unsigned short	idr[LENARR];
	short		idx[LENARR];
	short		irn[LENARR];
	short		ib1[LENARR];
	short		ib2[LENARR];
} OSdata;

/* Structures for reading the high resolution data from the file */
typedef struct osdata_hi {
	short		itm[LENTIM];
	short		lat[LENARR_HI];
	unsigned short	lon[LENARR_HI];
	unsigned short	iql[LENARR_HI];
	short		isp[LENARR_HI];
	unsigned short	idr[LENARR_HI];
	short		idx[LENARR_HI];
	short		irn[LENARR_HI];
	short		ib1[LENARR_HI];
	short		ib2[LENARR_HI];
} OSdata_hi;

typedef struct osplot {
	int	nplot;
	int	icolr;
	float	value;
	float	rlat[LENPLT];
	float	rlon[LENPLT];
	float	sped[LENPLT];
	float	drct[LENPLT];
} OSplot;

/* Internal function prototypes */
Boolean OSrainCheck (    short rain_flag,
			 int iflag );

Boolean OStimeCheck    ( short itime[],
			 char *dtime,
			 char *stime,
			 char *etime );

/*=====================================================================*/

void gg_osrd ( char *filtyp, char *filnam, char *stime, char *etime, 
     char *dtime, float fwninc[], int icolrs[], int icolrs2[],
     int *numclr, int *iskip, int *interv, int *itmclr, int iflags[],
     int *ityp, int *iret )
/************************************************************************
 * gg_osrd								*
 *									*
 * This routine reads the data from a NESDIS OSCAT file.		*
 *									*
 * gg_osrd ( filtyp, filnam, stime, etime, dtime, fwninc, icolrs, 	*
 *	     icolrs2, numclr, iskip, interv, itmclr, iflags, ityp,	*
 *	     iret )							*
 *									*
 * Input parameters:							*
 *	*filtyp		char		Data type			*
 *	*filnam		char		Data file name			*
 *	*stime		char		Start time of range		*
 *	*etime		char		End time of range		*
 *	*dtime		char		File time			*
 *	fwninc []	float		Wind speed break points		*
 *	icolrs []	int		Wind speed colors		*
 *      icolrs2[]       int             The second colors for wind      *
 *	*numclr		int		Number of colors		*
 *	*iskip		int		Skip value			*
 *	*interv		int		Time stamp interval		*
 * 	*itmclr		int		Time stamp color		*
 *	iflags []	int		Flag values for plotting data	*
 *	*ityp		int		Type of wind vector		*
 *					  1  Directional arrow		*
 *					  2  Wind barb			*
 *					  3  Selected directional arrow	*
 *					  4  Selected regular arrow	*
 *					  5  Selected wind arrow	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * G. McFadden/IMSG	 9/10  	Adapted from gg_asrd			*
 * G. McFadden/IMSG	 1/12  	Added OSCAT high resolution data	*
 ***********************************************************************/
{
	int		i, j, k, ip, np, nbin, jgrp, incr, ktime0, ktime, 
			ntime, iostat, lincnt, ier, ier1, k2, jgrp2, np2;
	int		istart, iend, numc, iyoff, ixloff, ixroff;
	int		jflag, nump, ipos, nlimt, ier2;
	float		tspd, tspd2, xp[2], yp[2], wlon, rotat;
	char		textstr[4];
	Boolean		pltflg, pltflg2, proc;

	float	        skycode[LENPLT];
	int	        iskxoff[LENPLT], iskyoff[LENPLT];

	union OSinput {
	    OSdata	data;
	    int		iarr[LENINT];
	} input;

	union OSinput_hi {
	    OSdata_hi	data_hi;
	    int		iarr_hi[LENINT_HI];
	} input_hi;

        OSdata_hi pdata;

	OSplot		plot[NUMPLT], plot2[NUMPLT];

	FILE		*fptr;

/*---------------------------------------------------------------------*/

	*iret = 0;
	k = 0;
	numc = *numclr;
	if  ( numc > NUMPLT )  {
	    numc = NUMPLT;
	}

	for ( i = 0; i < numc; i++ ) {
	    plot[i].value = fwninc[i];
	    plot[i].icolr = icolrs[i];
	    plot[i].nplot = 0;

            plot2[i].value = fwninc[i];
            if ( iflags[RNFCLR] )
                plot2[i].icolr = icolrs2[i];
            else
                plot2[i].icolr = icolrs[i];
            plot2[i].nplot = 0;
	}

/* 
 *  Set Sky codes and offsets
 */
	for ( i = 0; i < LENPLT; i++ ) {
	    skycode[i]=0.0;
	    iskxoff[i]=0;
            iskyoff[i]=0;
	}

/*
 * Set the skip factor.
 */
	if  ( *iskip <= 0 ) {
	    incr = 1;
	}
	else {
	    incr = *iskip + 1;
	}

/*
 * Open the data file.
 */
	fptr = cfl_ropn ( filnam, NULL, &ier );

	iostat = 0;
	lincnt = 0;
	ktime0 = -9999;
	iyoff = 0;
	rotat = 0.0F;
	wlon = 60.0F;	
	jflag = 1;
	while  ( iostat == 0 )  {

/*
 * Read the data into the structure.
 */
	    cst_srch ( 0, (int)strlen(filtyp), "_HI", filtyp, &ipos, &ier2 );
	  
            if ( ier2 == -4 ) {

            	cfl_read ( fptr, LENREAD, (unsigned char *)(input.iarr),
	        	   &nbin, &iostat );
	    	nump = LENARR; 
	    	nlimt = NLIMIT;

/*
 * Swap the data if necessary.
 */

	        if  ( MTMACH == MTULTX ||
	    	      MTMACH == MTALPH ||
		      MTMACH == MTLNUX )  {
		    ip = LENINT;
		    ier = mv_swp2 ( &ip, input.iarr, input.iarr );
		}

/*
 * Set values for the processing structure.
 */
	    	for ( i = 0; i < LENTIM; i++ ) {
                    pdata.itm[i] = input.data.itm[i];
	    	}

	    	for ( i = 0; i < nump; i++ ) {
		    pdata.lat[i] = input.data.lat[i];
		    pdata.lon[i] = input.data.lon[i];
		    pdata.iql[i] = input.data.iql[i];
		    pdata.isp[i] = input.data.isp[i];
		    pdata.idr[i] = input.data.idr[i];
		    pdata.idx[i] = input.data.idx[i];
		    pdata.irn[i] = input.data.irn[i];
		    pdata.ib1[i] = input.data.ib1[i];
		    pdata.ib2[i] = input.data.ib2[i];
            	}

            }
	    else {

            	cfl_read ( fptr, LENREAD_HI, (unsigned char *)(input_hi.iarr_hi),
	        	   &nbin, &iostat );
	    	nump = LENARR_HI; 
	    	nlimt = NLIMIT_HI;
/*
 * Swap the data if necessary.
 */
	        if  ( MTMACH != MTULTX &&
	    	      MTMACH != MTALPH &&
		      MTMACH != MTLNUX )  {
		    ip = LENINT;
		    ier = mv_swp2 ( &ip, input_hi.iarr_hi, input_hi.iarr_hi );
		}

/*
 * Set values for the processing structure.
 */
	    	for ( i = 0; i < LENTIM; i++ ) {
                    pdata.itm[i] = input_hi.data_hi.itm[i];
	    	}

	    	for ( i = 0; i < nump; i++ ) {
		    pdata.lat[i] = input_hi.data_hi.lat[i];
		    pdata.lon[i] = input_hi.data_hi.lon[i];
		    pdata.iql[i] = input_hi.data_hi.iql[i];
		    pdata.isp[i] = input_hi.data_hi.isp[i];
		    pdata.idr[i] = input_hi.data_hi.idr[i];
		    pdata.idx[i] = input_hi.data_hi.idx[i];
		    pdata.irn[i] = input_hi.data_hi.irn[i];
		    pdata.ib1[i] = input_hi.data_hi.ib1[i];
		    pdata.ib2[i] = input_hi.data_hi.ib2[i];
            	}

	    }

/*
 * Check the line count against the skip factor. If the
 * line count is a multiple of the increment, then process
 * the line of data. Otherwise, skip the line.
 */
            if  ( lincnt % incr == 0 )  {
                proc = TRUE;
            }
            else {
                proc = FALSE;
            }
            lincnt++;

/*
 * plot time stamps.
 */
	    if ( *itmclr > 0 && iostat == 0 ) {
	        ktime = (int)pdata.itm[1] * 60 + (int)pdata.itm[2];
	        if ( (ktime % *interv) == 0 && ktime != ktime0 && 
                     pdata.itm[0] != IMISSD &&
	             OStimeCheck(pdata.itm, dtime, stime, etime) ) {

		    istart = 0;
		    while ( istart < nump && ( pdata.isp[istart] == IMISSD ||
                                 pdata.isp[istart] <= 0 ) )
		    {
		        istart++;
		    }

		    iend = nump - 1;
		    while ( iend >= 0 && ( pdata.isp[iend] == IMISSD || 
                                 pdata.isp[iend] <= 0 ) )
		    {
                        iend--;
                    }

		    if ( istart < iend ) { 
	    	        xp[0] = (float)pdata.lat[istart]/100.0F;
	    	        yp[0] = (float)(pdata.lon[istart])/100.0F;
	    	        xp[1] = (float)pdata.lat[iend] / 100.0F;
	    	        yp[1] = (float)(pdata.lon[iend]) / 100.0F;

/*
 * Set the offset for text.
 */
                        if ( yp[0] < yp[1] ) {
                            ixloff = -8;
                            ixroff = 1;
                        }
                        else {
                            ixloff = 1;
                            ixroff = -8;
                        }

                        if ( (yp[0] > 360.0F - wlon) && (yp[1] < wlon) ) {
                            ixloff = -8;
                            ixroff = 1;
                        }
                        if ( (yp[1] > 360.0F - wlon) && (yp[0] < wlon) ) {
                            ixloff = 1;
                            ixroff = -8;
                        }

/*
 * The longitude is stored as 0 -> 360.  PRNLON will correct the longitude to
 * the range -180 -> 180.
 */
	    	        np2 = 1;
            	        prnlon ( &np2, &(yp[0]), &ier );
            	        prnlon ( &np2, &(yp[1]), &ier );

		        np = 2;
			gscolr ( itmclr, &ier); 
		        gline(sys_M, &np, xp, yp, &ier, strlen(sys_M));

		        ntime = (int)pdata.itm[1] * 100 + (int)pdata.itm[2];
            	        sprintf(textstr, "%4.4d", ntime); 

		        gtext  ( sys_M, &(xp[0]), &(yp[0]), textstr, &rotat, &ixloff, &iyoff,
                                 &ier, strlen(sys_M), strlen(textstr) );
		        gtext  ( sys_M, &(xp[1]), &(yp[1]), textstr, &rotat, &ixroff, &iyoff,
                                 &ier, strlen(sys_M), strlen(textstr) );

		        ktime0 = ktime;
		     }
   	    	}	

	    }
/*
 * This is where we finish plotting the time stamps and begin the process
 * of plotting the winds themselves.
 */
	    if  ( iostat == 0 && proc )  {

	    	if  ( pdata.itm[0] != IMISSD && OStimeCheck(pdata.itm, dtime, stime, etime) )  {

/*
 * Convert the data to the correct units and ranges.
 */
		   for  ( i = 0; i < nump; i+=incr )  {
			
		     if  ( pdata.isp[i] != IMISSD && pdata.isp[i] > 0 &&
				OSrainCheck(pdata.irn[i],iflags[RNFAIL]) )  {
/*
 * There is wind data in this cell and the user wants to plot this data.
 * This data will be plotted.
 */
		        if ( pdata.irn[i] == ISRAIN ) {
/*
 * This is OSCAT Rain flagged data and the user wants OSCAT Rain flagged data plotted, possibly in a second color.
 * Get the wind speed and convert from m/s to kts.
 */

			    tspd2 = (float) pdata.isp[i] / 100.0F;

			    tspd2 = pr_mskn ( &tspd2 );

			    jgrp2 = numc - 1;
			    for  ( j = numc - 1; j >= 0; j-- )  {
				if  ( tspd2 <= plot2[j].value )  {
				    jgrp2 = j;
				}
			    }

			    k2 = plot2[jgrp2].nplot;

			    plot2[jgrp2].rlat[k2] =
			    	(float) pdata.lat[i] / 100.0F;

/*
 * The longitude is stored as 0 -> 360.  PRNLON will correct the longitude to
 * the range -180 -> 180.
 */
			    plot2[jgrp2].rlon[k2] =
			    	(float) (pdata.lon[i]) / 100.0F;
			    np2 = 1;
			    prnlon ( &np2, &(plot2[jgrp2].rlon[k2]), &ier );

/*
 * Get the wind speed.
 */
			    plot2[jgrp2].sped[k2] = tspd2;

/*
 * The direction is stored in Oceanographic terms. It is 180 degrees 
 * out of phase with the Meteorological direction.  
 */
			    plot2[jgrp2].drct[k2] =
				((float)pdata.idr[i]/100.0F) + 180.0F;
                          
			    if  ( plot2[jgrp2].drct[k2] >= 360.0F )  {
				plot2[jgrp2].drct[k2] -= 360.0F;
			    }

			    (plot2[jgrp2].nplot)++;

			 }
			 else {

/*
 * This is not OSCAT Rain flagged data.
 * Get the wind speed and convert from m/s to kts.
 */

                            tspd = (float) pdata.isp[i] / 100.0F;

                            tspd = pr_mskn ( &tspd );

                            jgrp = numc - 1;
                            for  ( j = numc - 1; j >= 0; j-- )  {
                                if  ( tspd <= plot[j].value )  {
                                    jgrp = j;
                                }
                            }

                            k = plot[jgrp].nplot;

                            plot[jgrp].rlat[k] =
                                (float) pdata.lat[i] / 100.0F;

/*
 * The longitude is stored as 0 -> 360.  PRNLON will correct the longitude to
 * the range -180 -> 180.
 */
                            plot[jgrp].rlon[k] =
                                (float) (pdata.lon[i])/ 100.0F;
                            np = 1;
                            prnlon ( &np, &(plot[jgrp].rlon[k]), &ier );

/*
 * Get the wind speed.
 */
                            plot[jgrp].sped[k] = tspd;

/*
 * The direction is stored in Oceanographic terms. It is 180 degrees 
 * out of phase with the Meteorological direction.
 */
                            plot[jgrp].drct[k] =
                                ((float)pdata.idr[i]/100.0F) + 180.0F;

                            if  ( plot[jgrp].drct[k] >= 360.0F )  {
 				plot[jgrp].drct[k] -= 360.0F;
			    }

                            (plot[jgrp].nplot)++;

			 }
		      }

/*
 * Check the buffer sizes. If any are greater
 * than the limit, then plot all buffered data.
 */
			pltflg = FALSE;
			pltflg2 = FALSE;
			for  ( j = 0; j < numc; j++ )  {
			    if  ( plot[j].nplot > nlimt )  {
				pltflg = TRUE;
			    }
			}

		        for  ( j = 0; j < numc; j++ )  {
                            if  ( plot2[j].nplot > nlimt )  {
                                pltflg2 = TRUE;
                            }
                        }

			if  ( pltflg )  {
			    for  ( j = 0; j < numc; j++ )  {
				if  ( plot[j].icolr != 0 )  {
				    gscolr ( &(plot[j].icolr), &ier );

				    if ( *ityp == 2 || *ityp == 5 ) {
				        gbarb  ( sys_M, &(plot[j].nplot),
					     plot[j].rlat, plot[j].rlon,
					     plot[j].sped, plot[j].drct,
					     &ier, strlen(sys_M) );
				    }
				    else if ( *ityp == 4 ) {
				        garrw  ( sys_M, &(plot[j].nplot),
					     plot[j].rlat, plot[j].rlon,
					     plot[j].sped, plot[j].drct,
					     &ier, strlen(sys_M) );
				    }
				    else {
				        gdarr  ( sys_M, &(plot[j].nplot),
					     plot[j].rlat, plot[j].rlon,
					     plot[j].drct,
					     &ier, strlen(sys_M) );
				    }
				}
				plot[j].nplot = 0;
			    }
			}

			if  ( pltflg2 )  {
                            for  ( j = 0; j < numc; j++ )  {
                                if  ( plot2[j].icolr != 0 )  {
                                    gscolr ( &(plot2[j].icolr), &ier );
                                    if ( iflags[CIRCLE] ) 
                                        gsky ( sys_M, &(plot2[j].nplot),
	                                       skycode,
                                               plot2[j].rlat, plot2[j].rlon,
	                                       iskxoff, iskyoff,
                                               &ier, strlen(sys_M) );
                                             
				    if ( *ityp == 2 || *ityp == 5 ) {
                                        gbarb  ( sys_M, &(plot2[j].nplot),
                                             plot2[j].rlat, plot2[j].rlon,
                                             plot2[j].sped, plot2[j].drct,
                                             &ier, strlen(sys_M) );
				    }
				    else if ( *ityp == 4) {
                                        garrw  ( sys_M, &(plot2[j].nplot),
                                             plot2[j].rlat, plot2[j].rlon,
                                             plot2[j].sped, plot2[j].drct,
                                             &ier, strlen(sys_M) );
				    }
				    else {
                                        gdarr  ( sys_M, &(plot2[j].nplot),
                                             plot2[j].rlat, plot2[j].rlon,
                                             plot2[j].drct,
                                             &ier, strlen(sys_M) );
				    }
                                }
                                plot2[j].nplot = 0;
                            }
                        }

		    }

	    	}

	    }

	}

/*
 * Flush the buffers.
 */
	for  ( j = 0; j < numc; j++ )  {
	    gscolr ( &(plot[j].icolr), &ier );
	    if ( *ityp == 2 || *ityp == 5 ) {
	        gbarb  ( sys_M, &(plot[j].nplot), plot[j].rlat,
	    	     plot[j].rlon, plot[j].sped, plot[j].drct,
		     &ier, strlen(sys_M) );
	    }
	    else if ( *ityp == 4 ) {
	    	garrw  ( sys_M, &(plot[j].nplot), plot[j].rlat,
	    	     plot[j].rlon, plot[j].sped, plot[j].drct,
		     &ier, strlen(sys_M) );
	    }
	    else {
	        gdarr  ( sys_M, &(plot[j].nplot), plot[j].rlat,
	    	     plot[j].rlon, plot[j].drct, &ier, strlen(sys_M) );
	    }
	    gscolr ( &(plot2[j].icolr), &ier );
            if ( iflags[CIRCLE] ) 
                gsky ( sys_M, &(plot2[j].nplot), skycode,
                       plot2[j].rlat, plot2[j].rlon,
	               iskxoff, iskyoff, &ier, strlen(sys_M) );
	    if ( *ityp == 2 || *ityp == 5 ) {
                gbarb  ( sys_M, &(plot2[j].nplot), plot2[j].rlat,
                     plot2[j].rlon, plot2[j].sped, plot2[j].drct,
                     &ier, strlen(sys_M) );
	    }
	    else if ( *ityp == 4 ) {
                garrw  ( sys_M, &(plot2[j].nplot), plot2[j].rlat,
                     plot2[j].rlon, plot2[j].sped, plot2[j].drct,
                     &ier, strlen(sys_M) );
	    }
	    else {
                gdarr  ( sys_M, &(plot2[j].nplot), plot2[j].rlat,
                     plot2[j].rlon, plot2[j].drct, &ier, strlen(sys_M) );
	    }
	}

/*
 * Close the file.
 */
	cfl_clos ( fptr, &ier );

}

/*=====================================================================*/

Boolean OSrainCheck ( short rain_flag, int iflag )
/************************************************************************
 * OSrainCheck								*
 *									*
 * This function uses the data rain flag and the user input to 		*
 * determine whether to plot the current wind data.			*
 *									*
 * Boolean OSrainCheck ( rain_flag, iflag )				*
 *									*
 * Input parameters:							*
 *	rain_flag	short		Data rain flag			*
 *	iflag		int		User flag for plotting data	*
 *									*
 * Return parameters:							*
 *	OSrainCheck	Boolean		Indicator for rained wind	*
 *									*
 **									*
 * Log:									*
 * G. McFadden/SAIC	 9/10		Adapted from ASqualityCheck	*
 ***********************************************************************/
{

	Boolean		retval;

/*---------------------------------------------------------------------*/

	/*
	 * Assume that the wind will be plotted.
	 */
	retval = TRUE;

	/*
	 * If the wind is flagged as rained (rained wind) and
	 * the user does not want rained winds plotted, then
	 * set the return to false.
	 */
	if  ( rain_flag == ISRAIN )  {
	    if  ( iflag == 0 )  {
	    	retval = FALSE;
	    }
	}

	return ( retval );

}

/*=====================================================================*/

Boolean OStimeCheck ( short itime[], char *dtime, char *stime, char *etime )
/************************************************************************
 * OStimeCheck								*
 *									*
 * This function uses the start, end and file times to determine if the	*
 * data is within the time range and may be plotted.			*
 *									*
 * Boolean OStimeCheck ( itime, dtime, stime, etime )			*
 *									*
 * Input parameters:							*
 *	itime[]		short		Data time array			*
 *	*dtime		char		File name time			*
 *	*stime		char		Start time of range		*
 *	*etime		char		End time of range		*
 *									*
 * Return parameters:							*
 *	OStimeCheck	Boolean		Indicator for valid time	*
 *									*
 **									*
 * Log:									*
 * G. McFadden/SAIC	 9/10	Adapted from QStimeCheck		*
 ***********************************************************************/
{

	int		itarr[5], jtarr[5], idarr[3], itmar[4],
			iyear, jday, lens, nmin1, nmin2, i, ier;

	dattm_t		ctime;

/*---------------------------------------------------------------------*/

	for ( i = 0; i < 4; i++ ) {
	    itmar[i] = (int) itime[i];
	}

	/*
	 * Get the file name time as a Julian day of the year.
	 * If the data time is less than the file time, then the data
	 * has crossed a year boundary, increase the year.
	 */
	ti_ctoi ( dtime, itarr, &ier, strlen(dtime) );
	ti_itoj ( itarr, &iyear, &jday, &ier );

	if  ( itime[0] < jday )  {
	    iyear ++;
	}

	/*
	 * Set the data time string.
	 */
	ti_jtoi ( &iyear, &(itmar[0]), idarr, &ier );

	jtarr[0] = idarr[0];
	jtarr[1] = idarr[1];
	jtarr[2] = idarr[2];
	jtarr[3] = itmar[1];
	jtarr[4] = itmar[2];

	ti_itoc ( jtarr, ctime, &ier, sizeof(ctime) );

	st_null ( ctime, ctime, &lens, &ier,
		  sizeof(ctime), sizeof(ctime) );

	/*
	 * Compute the differences between the times to determine
	 * if the data falls in the time range.
	 */
	ti_diff ( ctime, stime, &nmin1, &ier,
		  strlen(ctime), strlen(stime) );
	ti_diff ( etime, ctime, &nmin2, &ier,
		  strlen(etime), strlen(ctime) );

	if  ( nmin1 * nmin2 > 0 )  {
	    return ( TRUE );
	}
	else {
	    return ( FALSE );
	}

}
