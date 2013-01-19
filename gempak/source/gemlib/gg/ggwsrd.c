#include "geminc.h"
#include "gemprm.h"

/* Useful parameters */
#define LENTIM	   4
#define LENARR	  79

#define IMISSW	-999

#define LENPLT	5000
#define NUMPLT	  20
#define NLIMIT	  ( LENPLT - LENARR - 1 )

#define RAIN    0
#define RAINCLR 1
#define CIRCLE  2

/* Structures for reading the data from the file */
typedef struct wsdata {
	short		itm[LENTIM];
	short		lat[LENARR];
	short		lon[LENARR];
	unsigned short	iql[LENARR];
	short		isp[LENARR];
	unsigned short	idr[LENARR];
	short		irr[LENARR];
	short		sst[LENARR];
	short		tpw[LENARR];
	short		clw[LENARR];
} WSdata;

typedef struct wsplot {
	int	nplot;
	int	icolr;
	float	value;
	float	rlat[LENPLT];
	float	rlon[LENPLT];
	float	sped[LENPLT];
	float	drct[LENPLT];
} WSplot;

/* Internal function prototypes */
Boolean WSqualityCheck ( unsigned short iqual,
			 int iflags[] );

Boolean WStimeCheck    ( short itime[],
			 char *dtime,
			 char *stime,
			 char *etime );
Boolean WSrainCheck ( unsigned short iqual);

void swap_WS ( WSdata *);

void swap_bytes(short int[], int);

void swap_bytes_unsigned(unsigned short int[], int);


/************************************************************************
 * ggwsrd.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

/*=====================================================================*/

void gg_wsrd ( char *filtyp, char *filnam, char *stime, char *etime, 
     char *dtime, float fwninc[], int icolrs[], int icolrs2[],
     int *numclr, int *iskip, int *interv, int *itmclr, int iflags[],
     int *ityp, int *iret )
/************************************************************************
 * gg_wsrd								*
 *									*
 * This routine reads the data from a NESDIS WindSAT file.		*
 *									*
 * gg_wsrd ( filtyp, filnam, stime, etime, dtime, fwninc, icolrs, 	*
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
 * G. McFadden/SAIC	06/06   Adapted from ggqsrd.c			*
 *									*
 ***********************************************************************/
{

	int		i, j, k, np, nbin, jgrp, incr, ktime0, ktime, 
			ntime, iostat, lincnt, ier, k2, jgrp2;
	int		istart, iend, numc, iyoff, ixloff, ixroff;
	int		jflags[2], nump, nlimt;
/*	int		irow;  */
	float		tspd, tspd2, xp[2], yp[2], wlon, rotat;
	char		textstr[4];
	Boolean		pltflg, pltflg2, proc;

	float	        skycode[LENPLT];
	int	        iskxoff[LENPLT], iskyoff[LENPLT];

        char granpointer[51];
        char reclen[5];

        int ferr, ws_eof, nrec = 0;

	WSdata pdata;

	WSplot		plot[NUMPLT], plot2[NUMPLT];

	FILE		*fptr;

/*---------------------------------------------------------------------*/
	*iret = 0;
	k = 0;
	/*
	 * 
	 */
	numc = *numclr;
	if  ( numc > NUMPLT )  {
	    numc = NUMPLT;
	}

	for ( i = 0; i < numc; i++ ) {
	    plot[i].value = fwninc[i];
	    plot[i].icolr = icolrs[i];
	    plot[i].nplot = 0;

            plot2[i].value = fwninc[i];
            if ( iflags[RAINCLR] )
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

        if (fptr == NULL) {
           return;
        }

        nbin = fread(&reclen,sizeof(reclen),1,fptr);

        nbin = fread(granpointer,sizeof(granpointer),1,fptr);
        granpointer[50] = '\0';


/*	irow = 0;  NOT used */
	iostat = 0;
	lincnt = 0;
	ktime0 = -9999;
	iyoff = 0;
	rotat = 0.0F;
	wlon = 60.0F;	
	for ( i =0; i <= RAINCLR; i++ ) {
	    jflags[i] = 1;
	}

	while  ( iostat == 0 )  {

	    /*
	     * Read the data into the structure.
	     */
            nbin = fread(&pdata,sizeof(WSdata),1,fptr);
            
            ferr = ferror(fptr);

            ws_eof = feof(fptr);

            if ( ferr!=0 || ws_eof != 0 || nbin !=1 )
            {
              iostat = 1;
            }

            nrec++;

            nump = LENARR; 
	    nlimt = NLIMIT;

	    /*
             * Swap the data if necessary.
             */
	    if  ( MTMACH != MTULTX &&
                  MTMACH != MTALPH &&
                  MTMACH != MTLNUX )  {
                swap_WS ( &pdata );
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
	             WStimeCheck(pdata.itm, dtime, stime, etime) ) {

		    istart = 0;
		    while ( ( pdata.isp[istart] == IMISSW ||
				 !WSqualityCheck(pdata.iql[istart],jflags) ) 
			    && istart < nump ) {
		        istart++;
		    }

		    iend = nump - 1;
		    while ( ( pdata.isp[iend] == IMISSW || 
		   		 !WSqualityCheck(pdata.iql[iend],jflags) )
			    && iend >= 0 ) {
                        iend--;
                    }

		    if ( istart < iend ) { 
	    	        xp[0] = (float)pdata.lat[istart]/100.0F;
	    	        yp[0] = (float)pdata.lon[istart]/100.0F;
	    	        xp[1] = (float)pdata.lat[iend] / 100.0F;
	    	        yp[1] = (float)pdata.lon[iend] / 100.0F;		


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

                        if ( (yp[0] + 180.0F > 360.0F - wlon) && (yp[1] + 180.0F < wlon) ) {
                            ixloff = -8;
                            ixroff = 1;
                        }
                        if ( (yp[1] + 180.0F > 360.0F - wlon) && (yp[0] + 180.0F < wlon) ) {
                            ixloff = 1;
                            ixroff = -8;
                        }

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

	    if  ( iostat == 0 && proc )  {

	    	if  ( WStimeCheck(pdata.itm, dtime, stime, etime) )  {

		   /*
		    * Convert the data to the correct units and ranges.
		    */
		   for  ( i = 0; i < nump; i+=incr )  {
			
		     if  ( pdata.isp[i] != IMISSW &&
				WSqualityCheck(pdata.iql[i],iflags) )  {

		        if ( WSrainCheck(pdata.iql[i]) && iflags[RAIN] ) {
 
			    /*
			     * Get the wind speed and convert from
			     * m/s to kts.
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
			     * The longitude is stored as -180 -> 180.
			     */
			    plot2[jgrp2].rlon[k2] =
			    	(float) pdata.lon[i] / 100.0F;

			    /*
			     * Get the wind speed.
			     */
                            if (tspd2>0.0F)
			        plot2[jgrp2].sped[k2] = tspd2;
                            else
			        plot2[jgrp2].sped[k2] = RMISSD;

			    /*
			     * The direction is stored in Oceanographic
			     * terms. It is 180 degrees out of phase
			     * with the Meteorological direction.
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
                             * Get the wind speed and convert from
                             * m/s to kts.
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
                             * The longitude is stored as -180 -> 180.
                             */
                            plot[jgrp].rlon[k] =
                                (float) pdata.lon[i] / 100.0F;

                            /*
                             * Get the wind speed.
                             */
                            if (tspd>0.0F)
                               plot[jgrp].sped[k] = tspd;
                            else
			       plot[jgrp].sped[k] = RMISSD;

                            /*
                             * The direction is stored in Oceanographic
                             * terms. It is 180 degrees out of phase
                             * with the Meteorological direction.
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

Boolean WSqualityCheck ( unsigned short iqual, int iflags[] )
/************************************************************************
 * WSqualityCheck							*
 *									*
 * This function uses the data quality flag and the user input to 	*
 * determine whether to plot the current wind data.			*
 *									*
 * Boolean WSqualityCheck ( iqual, iflags )				*
 *									*
 * Input parameters:							*
 *	iqual		unsigned short	Data quality flag		*
 *	iflags []	int		User flags for plotting data	*
 *									*
 * Return parameters:							*
 *	WSqualityCheck	Boolean		Indicator for valid wind	*
 *									*
 **									*
 * Log:									*
 * G. McFadden/SAIC	 6/06	Adapted from QSqualityCheck		*
 ***********************************************************************/
{

	int		i, ibits[16];
	unsigned short 	jq;
	int		mask = 1 << 15;

	Boolean		retval;

/*---------------------------------------------------------------------*/

	/*
	 * If the data quality flag is 0, then the wind is ok, 
	 * and there is nothing else to check.
	 */
	if  ( iqual == 0 )  {
	    return ( TRUE );
	}

	/*
	 * Get the bit values for the data quality flag.
	 */
	jq = iqual;
	for  ( i = 15; i >= 0; i-- )  {
	    ibits[i] = ( ( ( jq & mask ) == 0 ) ? 0 : 1 );
	    jq <<= 1;
	}

	/*
	 * If any of bits 0-9 are on, then do not plot this wind.
	 *
	for  ( i = 0; i < 10; i++ )  {
	    if  ( ibits[i] == 1 )  {
		return ( FALSE );
	    }
	}*/

	/*
	 * If the ice bit is on, then do not plot this wind.
	 */
	if  ( ibits[2] == 1 )  {
	    return ( FALSE );
	}

	/*
	 * If the land bit is on, then do not plot this wind.
	 */
	if  ( ibits[3] == 1 )  {
	    return ( FALSE );
	}

	/*
	 * Assume that the wind will be plotted.
	 */
	retval = TRUE;

	return ( retval );

}

/*=====================================================================*/
Boolean WSrainCheck ( unsigned short iqual )
/************************************************************************
 * WSrainCheck                                                       	*
 *                                                                      *
 * This function uses the data quality flag determine whether the 	*
 * current wind data is the rain data.                     		*
 *                                                                      *
 * Boolean WSrainCheck ( iqual )                             		*
 *                                                                      *
 * Input parameters:                                                    *
 *      iqual           unsigned short  Data quality flag               *
 *                                                                      *
 * Return parameters:                                                   *
 *      WSrainCheck  Boolean         Indicator for rained-wind        	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. McFadden/SAIC	06/06	Adapted from QSrainCheck		*
 ***********************************************************************/
{

        int             i, ibits[16];
        unsigned short  jq;
        int             mask = 1 << 15;

	Boolean         retval;

/*---------------------------------------------------------------------*/

        /*
         * If the data quality flag is 0, then the wind is non-rained. 
         */
	if  ( iqual == 0 )  {
            return ( FALSE );
        }


        /*
         * Get the bit values for the data quality flag.
         */
        jq = iqual;
        for  ( i = 15; i >= 0; i-- )  {
            ibits[i] = ( ( ( jq & mask ) == 0 ) ? 0 : 1 );
            jq <<= 1;
        }

	retval = FALSE;

	/* if  ( ibits[12] == 0 && ibits[13] == 1 ) retval = TRUE;   */
	if  ( ibits[0] == 1 || ibits[15] == 1 ) retval = TRUE;   

	return ( retval );

}

/*=====================================================================*/

Boolean WStimeCheck ( short itime[], char *dtime, char *stime, char *etime )
/************************************************************************
 * WStimeCheck								*
 *									*
 * This function uses the start, end and file times to determine if the	*
 * data is within the time range and may be plotted.			*
 *									*
 * Boolean WStimeCheck ( itime, dtime, stime, etime )			*
 *									*
 * Input parameters:							*
 *	itime[]		short		Data time array			*
 *	*dtime		char		File name time			*
 *	*stime		char		Start time of range		*
 *	*etime		char		End time of range		*
 *									*
 * Return parameters:							*
 *	WStimeCheck	Boolean		Indicator for valid time	*
 *									*
 **									*
 * Log:									*
 * G. McFadden/SAIC	 6/06	Adapted from QStimeCheck		*
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

/*=====================================================================*/
void swap_WS ( WSdata *pdata )
/************************************************************************
 * swap_WS                                                              *
 *                                                                      *
 * This function swaps the bytes of the data in a WindSAT structure.    *
 *                                                                      *
 * void swap_WS ( WSdata *pdata )                                       *
 *                                                                      *
 *                                                                      *
 * Input/Output parameter:                                              *
 *      WSdata   *pdata                Pointer to WS structure whose    *
 *                                      data is to be swapped.          *
 **                                                                     *
 * Log:                                                                 *
 * G. McFadden/SAIC      4/06                                           *
 ************************************************************************/
{
   swap_bytes(pdata->itm,LENTIM);
   swap_bytes(pdata->lat,LENARR);
   swap_bytes(pdata->lon,LENARR);
   swap_bytes_unsigned(pdata->iql,LENARR);
   swap_bytes(pdata->isp,LENARR);
   swap_bytes_unsigned(pdata->idr,LENARR);
   swap_bytes(pdata->irr,LENARR);
   swap_bytes(pdata->sst,LENARR);
   swap_bytes(pdata->tpw,LENARR);
   swap_bytes(pdata->clw,LENARR);

}

/*=====================================================================*/
void swap_bytes(short int swap_this_short[], int n)
/************************************************************************
 * swap_bytes                                                           *
 *                                                                      *
 * This function swaps the bytes of an arry of short ints.              *
 *                                                                      *
 * void swap_bytes ( swap_this_short[], n )                             *
 *                                                                      *
 * Input parameter:                                                     *       
 *      n               int             number of swaps to do.          *
 *                                                                      *
 * Input/Output parameter:                                              *
 *      swap_this_short short[]         Array whose bytes are swapped   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. McFadden/SAIC      4/06                                           *
 ************************************************************************/
{
   int i;

   union
   {
      short this_word;
      char buf[2];
   } t,v;

  for (i=0; i<n; i++)
  {
     t.this_word = swap_this_short[i];
     v.buf[0] = t.buf[1];
     v.buf[1] = t.buf[0];
     swap_this_short[i] = v.this_word;
  }
}

/*=====================================================================*/
void swap_bytes_unsigned(unsigned short int swap_this_short[], int n)
/************************************************************************
 * swap_bytes_unsigned                                                  *
 *                                                                      *
 * This function swaps the bytes of an arry of unsigned short ints.     *
 *                                                                      *
 * void swap_bytes_unsigned ( swap_this_short[], n )                    *
 *                                                                      *
 * Input parameter:                                                     *       
 *      n               int             number of swaps to do.          *
 *                                                                      *
 * Input/Output parameter:                                              *
 *      swap_this_short short[]         Array whose bytes are swapped   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. McFadden/SAIC      4/06                                           *
 ************************************************************************/
{
   int i;

   union
   {
      unsigned short this_word;
      char buf[2];
   } t,v;

  for (i=0; i<n; i++)
  {
     t.this_word = swap_this_short[i];
     v.buf[0] = t.buf[1];
     v.buf[1] = t.buf[0];
     swap_this_short[i] = v.this_word;
  }
}

