#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "clocmn.h"

#define	N2S	0
#define	W2E	1
#define	W2ELIM	0.1F
#define	VOR_FMT	0
#define	LATLON	1
#define	WSM	2
#define	VAA	3
#define	GFA_FMT	4
#define SEPARATOR_TO	4
#define SEPARATOR_DASH	5

#define GFA_TOL	0.0001F

void clo_from ( int vgtype, int reorder, int npin, int flag, float *lat, 
			float *lon, int maxchar, char *str, int *iret )
/************************************************************************
 * clo_from                                                    		*
 *                                                                      *
 * This function returns a "from" line given a series of lat-lon	*
 * coordinates.  The format of the "from" line is determined by vgtype.	*
 * The parameter reorder is an indicator whether the points consist of	*
 * an area which is closed and the points should be re-ordered in a	*
 * clockwise fashion, if necessary, and that the first point listed in	*
 * the "from" line is the northernmost point.  The flag parameter 	*
 * indicates whether lat-lon coordinates in International SIGMETs are to*
 * be formatted with direction prepended (flag==0) or with direction	*
 * postpended (flag==1)	or as VOR (flag==2).				*
 *                                                                      *
 * clo_from ( vgtype, reorder, npin, flag, lat, lon, maxchar,           *
 * 	      str, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	vgtype		int		VG type of "from" line		*
 *	reorder		int		VG reorder of "from" line	*
 *	npin		int		Number of points		*
 *	flag		int		Flag for coordinate format	*
 *	*lat		float		Latitudes			*
 *	*lon		float		Longitudes			*
 *	maxchar		int		Maximum number of chars in str	*
 *									*
 * Output parameters:                                                   *
 *	*str		char		"From" line string		*
 *	*iret		int		Return value			*
 *					=  0 - OK			*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 7/99	Create					*
 * D.W.Plummer/NCEP	 8/99	Add CONVSIG, NCONVSIG, CONVOLK & AIRMET	*
 * D.W.Plummer/NCEP	 9/99	Sort area types northernmost & clockwise*
 * M. Li/GSC		10/99	Modified clo_direct and clo_compass code*
 * A. Hardy/GSC         12/99   Added flag for lat/lon                  *
 * D.W.Plummer/NCEP	12/99	Added processing for WSM_ELM vgtype	*
 * F. J. Yen/NCEP	 8/00	Made intl sig lat/lon at least 4 digits *
 * D.W.Plummer/NCEP	 2/01	Changed units of WSM from NM to SM	*
 * D.W.Plummer/NCEP	 5/01	Simplified conversion of DD to DM	*
 * D.W.Plummer/NCEP	 5/01	Added chk of pt order for SIGTYP_LINE	*
 * D.W.Plummer/NCEP	 6/01	Change criteria for line point ordering	*
 * D.W.Plummer/NCEP	10/01	Change meaning of flag for intl sigmets	*
 * 				from dd or dms to pre or post ordinate	*
 * m.gamazaychikov/SAIC  9/02 	remove portion of the code duplicating  *
 *				function clo_reorder;			*
 *				add call to clo_reorder			*
 * S. Jacobs/NCEP	10/02	Increased np for area type		*
 * F. J. Yen/NCEP	 1/04	Handled VOR format for intl SIGMETs.	*
 *				Updated and corrected prolog about flag.*
 * J. Lewis/AWC		 3/05   Added chk for new from line format      *
 * J. Lewis/AWC		 6/05   remove reference to LLMXPT		*
 * B. Yin/SAIC		 6/05	increase indx size by 1 besause of np++	*
 * D.W.Plummer/NCEP	 7/05	Add NEW_VAA_LATLON_FORMAT and VAA type	*
 * S. Jacobs/NCEP	 9/05	Add break to WSM case before VAA	*
 * B. Yin/SAIC		10/05	Add separator flags for GFAs		*
 * B. Yin/SAIC		 1/06	remove the space around hyphen		*
 * D.W.Plummer/NCEP	11/06	Explicit processing for GFAs		*
 * D.W.Plummer/NCEP	01/07	clo_tmatch for GFAs, not clo_tclosest	*
 * K. Tyle/UAlbany      11/10   Increased dimension of prefs_tag	*	
 ***********************************************************************/
{
int	ii, jj, idist, np, ier, icmp;
float	dist, dir, minlat, maxlat, dlat;
char	tstr[8], id[9], dir16[4], prefs_tag[22];
char	vaafmt[20], vaasep[8];
int	*indx;
int     lattmp, lontmp;
int	line_order, reverse, format_type;
Boolean newcoord, newvaacoord;
int	n_nms, nclose;
char	nm[17];
float	GFAtol=GFA_TOL;

/*---------------------------------------------------------------------*/
 
	*iret = 0;

	str[0] = '\0';

	/*
	 * Check if the new coordinate format is to be used.
	 */
	strcpy ( prefs_tag, "NEW_LATLON_FORMAT" );
	ctb_pfbool ( prefs_tag, &newcoord, &ier );

	strcpy ( prefs_tag, "NEW_VAA_LATLON_FORMAT" );
	ctb_pfbool ( prefs_tag, &newvaacoord, &ier );
	
	/*
	 *  Allocate memory.
	 */
	G_MALLOC ( indx, int, npin + 1, "CLO_FROM" );

	np = npin;
	for ( jj = 0; jj < np; jj++ )  indx[jj] = jj;

	if ( reorder == SIGTYP_AREA )  {
            clo_reorder( np, lat, lon, indx, iret );
	    np++;
	}
	else if ( reorder == SIGTYP_LINE )  {

	    /*
	     *  If reorder is a line, re-order processing of
	     *  points to do either west-to-east or north-to-south.
	     *  West-to-east defined as all points within W2ELIM
	     *  degrees of one another.
	     */

            minlat = lat[0];
            maxlat = minlat;
            for ( jj = 1; jj < np; jj++ )  {
                minlat = G_MIN ( minlat, lat[jj] );
                maxlat = G_MAX ( maxlat, lat[jj] );
            }
            dlat = G_ABS( maxlat - minlat );

            line_order = N2S;
            if ( dlat <= W2ELIM )  line_order = W2E;

            reverse = G_FALSE;
            if ( line_order == N2S && lat[0] < lat[np-1] )
                reverse = G_TRUE;
            if ( line_order == W2E && lon[0] > lon[np-1] )
                reverse = G_TRUE;

            if ( reverse )  {
                for ( jj = 0; jj < np; jj++ )  indx[jj] = np-1-jj;
            }

	}

	/*
	 *  Set format_type.
	 */

	if ( vgtype == SIGINTL_ELM ) {
	    /*
	     *	International SIGMET
	     */
	    if ( flag != 2 )
		format_type = LATLON;
	      else
		format_type = VOR_FMT;
	  }
	  else if ( vgtype == SIGNCON_ELM || vgtype == SIGCONV_ELM ||
	    	    vgtype == SIGOUTL_ELM || vgtype == SIGAIRM_ELM )
	    /*
	     *	Non-Convective SIGMET, Convective SIGMET,
	     *	Convective Outlook
	     */
	    format_type = VOR_FMT;
	  else if ( vgtype == GFA_ELM )
	    /*
	     *	AIRMET
	     */
	    format_type = GFA_FMT;
	  else if ( vgtype == WSM_ELM )
	    /*
	     *	Watch Status Message
	     */
	    format_type = WSM;
	  else if ( vgtype == VOLC_ELM || vgtype == ASHCLD_ELM )
	    /*
	     *	VAA volcano and ash clouds.
	     */
	    format_type = VAA;
	  else
	    format_type = IMISSD;

	/*
	 *  Loop through all the points using the indx array.
	 */
		
	for ( jj = 0; jj < np; jj++ )  {

	    ii = indx[jj];

	    switch ( format_type )  {

		case	LATLON:		/* latitude/longitude display */
					/* eg., 3913N7705W 4134N8120W */
					/* eg., N3913W07705 N4134W08120 */

		    if ( jj != 0 )  strcat ( str, " " );

		    if ( flag == 0 ) {

		        if ( ( newcoord == G_TRUE ) && ( jj != 0 ) ) strcat ( str, "- " );
		        if ( lat[ii] >= 0.0F )
			    strcat ( str, "N" );
		        else
		            strcat ( str, "S" );

	                /*
	                 *  Convert degree, decimal to degree, minutes.
	                 */
		        lattmp = DDTODM ( G_ABS( lat[ii] ) );
		        sprintf( tstr, "%04d", lattmp );

		        strcat ( str, tstr );
		        if ( newcoord == G_TRUE )  strcat ( str, " " );

		        if ( lon[ii] >= 0.0F )
			    strcat ( str, "E" );
		        else
		            strcat ( str, "W" );

	                /*
	                 *  Convert degree, decimal to degree, minutes.
	                 */
		        lontmp = DDTODM ( G_ABS( lon[ii] ) );
		        sprintf( tstr, "%05d", lontmp );

		        strcat ( str, tstr );


		    }
		    else  {

	                /*
	                 *  Convert degree, decimal to degree, minutes.
	                 */
		        lattmp = DDTODM ( G_ABS( lat[ii] ) );

		        sprintf( tstr, "%04d", lattmp );

		        strcat ( str, tstr );

		        if ( lat[ii] >= 0.0F )
			    strcat ( str, "N" );
		        else
		            strcat ( str, "S" );

	                /*
	                 *  Convert degree, decimal to degree, minutes.
	                 */
		        lontmp = DDTODM ( G_ABS( lon[ii] ) );
		        sprintf( tstr, "%05d", lontmp );

		        strcat ( str, tstr );

		        if ( lon[ii] >= 0.0F )
			    strcat ( str, "E" );
		        else
		            strcat ( str, "W" );

		    }

		    break;

		case	VOR_FMT:	/* distance and 16-pt compass   */
					/* to closest VOR point		*/
					/* eg., 20SSW EMI TO 20ENE CLE	*/

		    clo_tdirect( "VOR", lat[ii], lon[ii], id, 
				 &dist, &dir, &ier );

		    clo_compass ( &dir, dir16, &icmp, &ier );

		    /*
		     *  Round distance to the nearest 10 nautical miles;
		     *  If convective outlook and less than 30 nm, set to 0.
		     */
		    idist = G_NINT ( dist * M2NM / 10.0F ) * 10;
		    if ( vgtype == SIGOUTL_ELM && idist < 30 )  idist = 0;

		    if ( jj > 0 )  {
			/*
			 *  Different separators for different products.
			 */
			if ( vgtype == SIGCONV_ELM || vgtype == SIGOUTL_ELM ||
			     vgtype == SIGINTL_ELM ) 
			    strcat ( str, "-" );
		        else if ( vgtype == SIGAIRM_ELM || vgtype == SIGNCON_ELM )
			    strcat ( str, " TO " );
		    }

		    if ( idist != 0 )  {
			sprintf( tstr, "%d", idist );
			strcat ( str, tstr );
			if ( vgtype == SIGINTL_ELM ) strcat ( str, " " );
			strcat ( str, dir16 );
		        strcat ( str, " " );
		    }

		    strcat ( str, id );

		    break;

		case	GFA_FMT:	/* closest SNAP point		*/

		    /*
		     * Use clo_tmatch since all points are already snapped
		     */
		    clo_tmatch( "SNAP", lat[ii], lon[ii], GFAtol, &ier );

		    if ( ier != 0 )  {
			nclose = 1;
			clo_tclosest( "SNAP", lat[ii], lon[ii], nclose, &ier );
		    }

		    clo_tgnm ( "SNAP", 1, (sizeof(nm)-1), &n_nms, nm, &ier );

		    cst_rpst ( nm, "_", " ", nm, &ier );

		    if ( jj > 0 )  {
		        if ( flag == SEPARATOR_TO )  {
			    strcat ( str, " TO " );
		        }
		        else if ( flag == SEPARATOR_DASH )  {
			    strcat ( str, "-" );
		        }
		    }

		    strcat ( str, nm );

		    break;

		case	WSM:		/* Watch status messages	*/
					/* SM distance and 16-pt compass*/
					/* to closest ANCHOR point	*/
					/* eg., 10 N DCA TO 20 NW HGR	*/

		    clo_tdirect( "ANCHOR", lat[ii], lon[ii], id, 
				 &dist, &dir, &ier );

		    clo_compass ( &dir, dir16, &icmp, &ier );

		    /*
		     *  Round distance to the nearest 5 statute miles.
		     */
		    idist = G_NINT ( dist * M2SM / 5.0F ) * 5;

		    if ( jj > 0 )  strcat ( str, " TO " );

		    if ( idist != 0 )  {
			sprintf( tstr, "%d ", idist );
			strcat ( str, tstr );
			strcat ( str, dir16 );
		        strcat ( str, " " );
		    }

		    strcat ( str, id );

		    break;

		case	VAA:		/* VAA volcano and ash clouds	*/

		    if ( newvaacoord == G_FALSE )  {
			strcpy ( vaafmt, "%s%04d%s%05d" );
			strcpy ( vaasep, " - " );
		    }
		    else if ( newvaacoord == G_TRUE )  {
			strcpy ( vaafmt, "%s%04d %s%05d" );
			strcpy ( vaasep, " - " );
		    }

	            /*
	             *  Convert degree, decimal to degree, minutes.
	             */
		    lattmp = DDTODM ( G_ABS( lat[ii] ) );
		    lontmp = DDTODM ( G_ABS( lon[ii] ) );
		    sprintf( tstr, vaafmt, 
			    ( lat[ii] >= 0.0F ) ? "N" : "S", lattmp,
			    ( lon[ii] >= 0.0F ) ? "E" : "W", lontmp );

		    strcat ( str, tstr );
		    if ( jj < (np-1) )  strcat ( str, vaasep );

		    break;

	    }

	}
        
	G_FREE ( indx, int );
	
	return;

}
