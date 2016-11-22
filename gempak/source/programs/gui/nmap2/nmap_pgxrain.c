#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

#define	NUMPROB	 5
#define	NOPROB	-1
#define MRGL     0
#define	SLGT	 1
#define	MDT	 2
#define	HIGH	 3
#define	EXCEED5	 4

typedef struct xrlineinfo
{
    char	closed;
    int		npts;
    float	*lat;
    float	*lon;
} XRLineInfo;

typedef struct xraininfo
{
    int		nline;
    XRLineInfo	*line;
} XRainInfo;

/************************************************************************
 * nmap_pgxrain.c                                                       *
 *									*
 * This module controls the XRAINF (Excessive Rainfall Potential	*
 * Outlook) prog creation.						*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *      pgxrain_update()      create XRAINF prog primary function	*
 *      pgxrain_udlist()      append information to xrainf prog text	*
 *      pgxrain_getfname()    build the xrainf prog text file name      *
 *	pgxrain_setltlnp()    set lat/lon arrays and output record	*
 *                                                                      *
 * Log:									*
 * F. J. Yen/NCEP	 3/99  Remove pgxrain_getfname. Use cvg_getfname*
 * T. Piper/SAIC	12/05  Made lat and lon of XRLineInfo & line	*
 *				of XRainInfo pointers.			*
 ***********************************************************************/

/*=====================================================================*/

void pgxrain_update ( char *fname, int *iret )
/************************************************************************
 * pgxrain_update                                                    	*
 *                                                                      *
 * This function creates the XRAINF prog text product from a vgf file.	*
 *                                                                      *
 * void pgxrain_update( fname, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*fname		char		VG Filename			*
 *                                                                      *
 * Output parameters:                                             	*
 *  	*iret		int		Return code			*
 *					 -1 = unable to convert		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	 9/98	Created					*
 * F. J. Yen/NCEP	12/98	Allowed any fill pattern.		*
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * S. Jacobs/NCEP	 4/01	Removed check for group type		*
 * J. Wu/SAIC		01/02	update the current layer only		*
 * S. Jacobs/NCEP	 3/03	Removed product header			*
 * S. Jacobs/NCEP	12/04	Look for spcl line w/ text in OUTLOOK gp*
 * T. Piper/SAIC	12/05	Made lat and lon pointers		*
 * T. Piper/SAIC	12/05	Initialize line pointers; move counter	*
 * T. Piper/SAIC	12/05	Initialize *lat and *lon		*
 * S. GUAN/NCEP          4/16   Added a MRGL contour.                   *
 ***********************************************************************/
{
int    		ier, ne, fpos, npts, cur_layer, el_layer;

int		grpid, totgrp, lowgrp, ng, iarr[10], nelm, prob;
int		ii, jj, kk, nn, knt;
char		grptyp, closed;
char		str[16];
float		*lat=NULL, *lon=NULL;

XRainInfo	xrain[NUMPROB];

VG_DBStruct     el;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Clear the text.
     */
    pgprd_clear();
    pgprd_putstr ( "\n", &ier );

    /*
     * Get the group id for OUTLOOK.
     */
    ces_gtgid ( "OUTLOOK", &grpid, &ier );
    grptyp = (char)grpid;

    /*
     * Get the first and last group numbers.
     */
    crg_ggnhl ( grptyp, &totgrp, &lowgrp, &ier );

    /*
     * Get the current layer.
     */
    cur_layer = pglayer_getCurLayer ();

    for ( jj = 0; jj < NUMPROB; jj++ ) {
    	xrain[jj].nline = 0;
	xrain[jj].line = NULL;
    }
    /*
     * Loop through the groups to find valid elements.
     */
    for ( ng = lowgrp; ng <= totgrp; ng++ ) {

    	/*
	 * Find the number of elements in this group.
	 */ 
	crg_gginx ( grptyp, ng, sizeof(iarr)/sizeof(iarr[0]),
		    iarr, &nelm, &ier );

	if  ( nelm > 0 )  {

	    /*
	     *  Loop through elements in this group.
	     */
	    npts = 0;
	    prob = NOPROB;
	    for ( ne = 0; ne < nelm; ne++ )  {

		/*
		 * Get the position and layer of the element.
		 */
		crg_goffset ( iarr[ne], &fpos, &ier );
		el_layer = crg_getLayer ( fpos );

		/*
		 * If this is a valid layer, check for valid elements.
		 */
		if ( el_layer == cur_layer && fpos > 0 ) {

		    /*
		     * Read the element.
		     */
		    cvg_rdrec ( fname, fpos, &el, &ier );

		    /*
		     * Find a special line and text grouped together.
		     */
		    switch ( (int)el.hdr.vg_type ) {
		    	case SPLN_ELM:
			    npts = el.elem.spl.info.numpts;
			    closed = el.hdr.closed;
			    G_MALLOC(lat, float, npts, "lat");
			    G_MALLOC(lon, float, npts, "lon");
			    /*
			     * Reverse the points if the line is
			     * "flipped". Otherwise, save the points.
			     */
			    if  ( el.elem.spl.info.spldir == -1)  {
				if  ( closed )  {
				    lat[0] = el.elem.spl.latlon[0];
				    lon[0] = el.elem.spl.latlon[npts];
				    nn = npts-1;
				    for ( ii = 1; ii < npts; ii++ ) {
					lat[ii] = el.elem.spl.latlon[nn];
					lon[ii] = el.elem.spl.latlon[npts+nn];
					nn--;
				    }
				}
				else {
				    nn = npts-1;
				    for ( ii = 0; ii < npts; ii++ ) {
					lat[ii] = el.elem.spl.latlon[nn];
					lon[ii] = el.elem.spl.latlon[npts+nn];
					nn--;
				    }
				}
			    }
			    else {
				for ( ii = 0; ii < npts; ii++ ) {
			    	    lat[ii] = el.elem.spl.latlon[ii];
				    lon[ii] = el.elem.spl.latlon[npts+ii];
				}
			    }
			    break;
		    	case SPTX_ELM:
			    cst_lcuc ( el.elem.spt.text, str, &ier );
			    switch ( str[0] )  {
			    	case 'H':
				    prob = HIGH;
				    break;
			    	case 'M':
                                    if (str[1] == 'R') prob = MRGL;
                                    if (str[1] == 'D') prob = MDT;
				    break;
			    	case 'S':
				    prob = SLGT;
				    break;
			    	case '5':
				    prob = EXCEED5;
				    break;
				default:
				    prob = NOPROB;
				    break;
			    	}
			    break;
			default:
			    break;
		    }
		}
	    }

	    if ( ( npts > 0 ) && ( prob != NOPROB ) )  {

		knt = xrain[prob].nline;
		(xrain[prob].nline)++;
		G_REALLOC(xrain[prob].line, XRLineInfo, xrain[prob].nline, "xrain[prob].line"); 
	    	xrain[prob].line[knt].npts = npts;
	    	xrain[prob].line[knt].closed = closed;
		G_MALLOC(xrain[prob].line[knt].lat, float, npts, "xrain[prob].line[knt].lat");
		G_MALLOC(xrain[prob].line[knt].lon, float, npts, "xrain[prob].line[knt].lon");
	
		for ( ii = 0; ii < npts; ii++ ) {
		    xrain[prob].line[knt].lat[ii] = lat[ii];
		    xrain[prob].line[knt].lon[ii] = lon[ii];
		}
	    }
	    G_FREE(lat, float);
	    G_FREE(lon, float);
	}
    }

    for ( jj = 0; jj < NUMPROB; jj++ ) {
        for ( kk = 0; kk < xrain[jj].nline; kk++ ) {
	    pgxrain_setltlnp ( jj,
	    		       xrain[jj].line[kk].lat,
			       xrain[jj].line[kk].lon,
	    		       xrain[jj].line[kk].npts,
			       xrain[jj].line[kk].closed,
			       iret );
	    G_FREE(xrain[jj].line[kk].lat, float);
	    G_FREE(xrain[jj].line[kk].lon, float);
	}
	G_FREE(xrain[jj].line, XRLineInfo);
    }
}

/*=====================================================================*/

void pgxrain_setltlnp ( int prob, float *elat, float *elon, int npts, 
						char closed, int *iret )
/************************************************************************
 * pgxrain_setltlnp                                                   	*
 *                                                                      *
 * This function sets up the lat/long arrays for the report from the	*
 * lat/long array from the vgf element and invokes pgxrain_udlist to	*
 * output the record.							*
 *                                                                      *
 * void pgxrain_setltlnp ( prob, elatlon, npts, closed, iret)		*
 *									*
 * Input parameters:                                                    *
 *	prob		int		probability of excessive rain	*
 *	*elatlon	float		latitude/longitude from vgf	*
 *	npts		int		number of points in elatlon	*
 *	closed		char		flag for closed line		*
 *									*
 * Output parameters:                                             	*
 *	*iret		int		Return code			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	 9/98	Created					*
 * F. J. Yen/NCEP	 9/98	Include endpoints if line is opened. 	*
 * S. Jacobs/NCEP	12/04	Changed input var raintyp to prob	*
 * T. Piper/SAIC	12/05	Made lat and lon pointers		*
 * S. Jacobs/NCEP	 3/06	Fixed crash: lat/lon had out-of-range	*
 * 				array for a closed line			*
 ***********************************************************************/
{
int    	ii;
float	*lat, *lon;
char	message[] = "WARNING--Line for Excessive Rainfall Potential "
	      "Outlook has less\n         than 2 vertices to put out\n";

/*---------------------------------------------------------------------*/
	*iret = 0;

	G_MALLOC(lat, float, npts, "lat");
	G_MALLOC(lon, float, npts, "lon");

        for ( ii = 0; ii < npts; ii++ )  {
	  lat[ii] = elat[ii];
	  lon[ii] = elon[ii];
        }

	if ( closed == (char)1 )   {
/*
 * Closed line; set last vertex to first
 */
	    npts++;
	    G_REALLOC(lat, float, npts, "lat");
	    G_REALLOC(lon, float, npts, "lon");
	    lat[npts-1] = elat[0];
	    lon[npts-1] = elon[0];
	}

/*
 * Put out records
 */
        pgxrain_udlist ( npts, prob, lat, lon, iret );
	if (*iret == 1) {
	    NxmWarn_show (mcanvw_getDrawingW(), message);
	}
        G_FREE(lat, float);
	G_FREE(lon, float);
}

/*=====================================================================*/

void pgxrain_udlist ( int npts, int prob, float *lat, float *lon, int *iret )
/************************************************************************
 * pgxrain_udlist                                                    	*
 *                                                                      *
 * This function finds the closest station in the lat lon arrays and	*
 * outputs the record for the line.					*
 *                                                                      *
 * void pgxrain_udlist ( npts, prob, lat, lon, iret )			*
 *									*
 * Input parameters:                                                    *
 *	npts		int		Number of vertices 		*
 *	prob		int		Rain probability		*
 *	*lat		float		latitude			*
 *	*lon		float		longitude			*
 *									*
 * Output parameters:                                             	*
 *	*iret		int		Return code			*
 *					 1  less than 2 vertices	*
 *					 2  Invalid prob value		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	 9/98	Created					*
 * F. J. Yen/NCEP	12/98	Modified exceed 5 text.			*
 * D.W.Plummer/NCEP	12/98	Change clo_tclsst to clo_tclosest	*
 * 				and how clo_tgid is called		*
 * D.W.Plummer/NCEP	 5/99	Added distance and direction to station	*
 * M. Li/GSC		10/99	Replaced clo_compass code		*
 * A. Hardy/GSC		01/00   Changed call for clo_tdirect		*
 * S. Jacobs/NCEP	 3/03	Added extra blank lines to product	*
 * S. Jacobs/NCEP	12/04	Changed input var raintyp to prob;	*
 * 				Changed wording of output text		*
 * S. GUAN/NCEP          4/16   Added a MRGL RISK                       *				
 ***********************************************************************/
{
int    	np, ier, nptout, recpos, distsm5, icmp;
int	maxreclen = 65;
float	distm, dir;
char	textstr[80], cmpdir[4];
char	id[20], idsav[20], stn[20];
char	raintext [] [20] = { "MARGINAL RISK ",
                             "SLIGHT RISK ",
			     "MODERATE RISK ",
			     "HIGH RISK " };

char	raintextend [] = "OF RAINFALL EXCEEDING FFG TO THE RIGHT OF A LINE FROM\n";

char	raintext5inch [] = "TOTAL RAINFALL AMOUNTS OF FIVE INCHES WILL BE POSSIBLE TO THE\nRIGHT OF A LINE FROM ";
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (npts < 2) {
	*iret = 1;
	return;
    }

    recpos = 0;
    switch ( prob ) {
    	case HIGH:
	case MDT:
	case SLGT:
        case MRGL:
	    recpos = 0;
	    pgprd_putstr (raintext[prob], &ier);
	    pgprd_putstr (raintextend, &ier);
	    break;
    	case EXCEED5:
	    recpos = 21;
	    pgprd_putstr (raintext5inch, &ier);
	    break;
    	default:
	    *iret = 2;
	    return;
    }

    idsav[0] =  '\0';
   
    nptout = 0;
    for ( np = 0; np < npts; np++ ) {

	/*
	 *  For each point, get distance, direction and stid
	 *  of closest station
	 */
	clo_tdirect( "SFSTNS", lat[np], lon[np], stn, &distm, &dir, &ier );

	/*
	 *  Convert numeric direction to 16-pt character compass
	 */
	clo_compass( &dir, cmpdir, &icmp, &ier );

	/*
	 *  Convert distance from meters to statute miles and
	 *  round to nearest 5
	 */
        distsm5 = G_NINT( distm * M2SM / 5.0F ) * 5;

        if ( distsm5 <= 5 )  {
	    /*
	     *  If distance is less than 5, print only station
	     */
	    sprintf( id, "%s", stn );
        }
	else  {
	    /*
	     *  Otherwise, print distance, 16-pt compass direction
	     *  and station
	     */
	    sprintf( id, "%d %s %s", distsm5, cmpdir, stn );
	}

	if (strcmp(id, idsav) != 0) {

	    strcpy (idsav, id);
	    recpos += strlen(id) + 1;

	    if ( recpos >= maxreclen ) {
	        sprintf(textstr, "\n" );
	        pgprd_putstr ( textstr, &ier );
	        recpos = strlen(id);
	    }
	    else {
		if  ( np != 0 )  {
		    pgprd_putstr ( " ", &ier);
		}
	    }

            pgprd_putstr ( id, &ier );
	    nptout++;
	}

    }
    if (nptout < 2)
    {
	*iret = 1;
    }
    pgprd_putstr ( ".\n\n", &ier);
}
