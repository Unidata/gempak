	SUBROUTINE GH_SPRB ( dattim, strnam, advno, grid, imax, jmax, 
     +			     curlat, curlon, hvgrid, dirmv, iret )
C************************************************************************
C* GH_SPRB 								*
C*									*
C* This subroutine creates the strike probablilty graphic for the TPC.  *
C*									*
C* GH_SPRB ( DATTIM, STRNAM, ADVNO, GRID, IMAX, JMAX, CURLAT, CURLON,   *
C*           HVGRID, DIRMV, IRET )					*
C*									*
C* Input parameters:							*
C*      DATTIM		CHAR*		Synoptic date/time string	*
C*      STRNAM		CHAR*		Storm identification		*
C*      ADVNO		CHAR*		Advisory number			*
C*	GRID(IMAX,JMAX) REAL		Grid data array			*
C*      IMAX		INTEGER		Maximum number of rows          *
C*      JMAX		INTEGER		Maximum number of columns       *
C*      CURLAT		REAL		Synoptic storm latitude		*
C*      CURLON		REAL		Synoptic storm longitude	*
C*	HVGRID		LOGICAL		Flag for presence of grid array *
C*	DIRMV		CHAR*		Direction of motion             *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	4/01 						*
C* D. Kidwell/NCEP	6/01 	Cleaned up                              *
C* A. Hardy/GSC		6/01	Added GSLINE, GQLINE			*
C* A. Hardy/GSC		6/01	Added color tag;map chck,GH_SAVE,GH_REST*
C* D. Kidwell/NCEP	8/01 	Changed 100 prob. to oval; added dirmv  *
C* A. Hardy/SAIC        8/01    Corrected garea for missing lat/lon	*
C* D. Kidwell/NCEP     10/01	Check for cur posn near edge; fix colors*
C* T. Lee/SAIC	       10/01	Added fill types to GCFILL calling seq.	*
C* D. Kidwell/NCEP	4/02 	Reworked garea calcs; added IP_SVAR;    *
C*				changed GSCNTR arg.; changed PS check   *
C* D. Kidwell/NCEP	5/02 	Changed name of land bounds file        *
C* T. Piper/SAIC	12/04	Added GG_SCAL				*
C* T. Piper/SAIC	01/05	Added title and mask color		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, strnam, advno, dirmv
	LOGICAL		hvgrid
        REAL		grid (imax,jmax) 
C*
	CHARACTER	cproj*72, latlon*50, noprob*100, bgsea*128, 
     +                  bgland*128, bglake*128, mapstr*128, ddev*12, 
     +                  cblk*2, coltag*33, cmap*3, prbclr (4)*15,
     +			mscale*100, title*48
C*
	PARAMETER 	( RLATLL = 1.,  RLONLL = -110.,
     +			  RLATUR = 60., RLONUR = -15.,  RDIST = 62.5 )
C*
	INTEGER		ifcolr (4), iflabl (4), ilcolr (4), ifltyp (4)
	LOGICAL		contur, mapok
	REAL		flvl (3), xlat (364), xlon (364), angle (4),
     +			ddist (5)
        INTEGER         ipclr(4)
C*
	INCLUDE		'ERMISS.FNC'
C*
	DATA	prbclr / ' ', 'p_prob_lt_20',
     +                   'p_prob_lt_50', 'p_prob_lt_100' /
	DATA	ipclr   / 0, 5, 21, 17 /
	DATA	title / 'Approx. Distance Scale ( Statute Miles )' /
C-----------------------------------------------------------------------
        mapok = .true.
	mscale = '32;6;1/SM/30/125/CC/.5;.07|Medium|' // title
C
C*    	Query currrent settings.
C
	CALL GH_SAVE ( ier )
C
	coltag = 'p_lat_lon_lines'
	CALL ST_LSTR ( coltag, lens, ier )
	CALL GH_COLR ( coltag(:lens), 26, ier)
	CALL GQCOLR ( icolr, ier)
	CALL ST_INCH ( icolr, cmap, ier )
	CALL ST_LSTR ( cmap, lens, ier )
	latlon = cmap(:lens) // '/1/1//5;5//2'
C
C*      Reverse black and white if device is for postscript.
C
	CALL GQDEV  ( ddev, iunit, iatyp, iret )
	IF  ( ddev (:2) .eq. 'PS' )  THEN
	    iwht = 32
	    iblk = 1
	  ELSE
	    iwht = 31
	    iblk = 32
	END IF 
C
C*	Set the text attributes.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
C* 	Define the coordinate system for the probability grid.
C
    	CALL GSGMAP ( 'CED', imax, jmax, RLATLL, RLONLL, RLATUR,
     +		      RLONUR, ier )
C
C*    	Query grid projection.
C
	CALL GQGPRJ ( cproj,  angle1, angle2, angle3, imx, imy,
     +		      dlatll, dlonll, dlatur, dlonur, iret )
C
C*	Get the non-zero row and column limits of the grid.
C
	IF ( hvgrid ) THEN
     	    CALL GH_SPRL ( grid, imax, jmax, ibeg, iend, jbeg, jend, 
     +	    		   ier1 )
	  ELSE
	    ier1 = -2
	END IF
	IF ( ier1 .ne. -2 ) THEN
	    zlonll = RLONLL + ibeg - 2
	    zlonur = RLONLL + iend
	    zlatll = jbeg - 1
	    zlatur = jend + 1
	    contur = .true.
	  ELSE
C
C*          Set default current lat/lon if found missing for garea fill.
C
            IF ( ( ERMISS ( curlat) ) .or. 
     +            ( ERMISS ( curlon ) ) ) THEN
                curlat = 25.
                curlon = -45.
            END IF
 	    zlonll = curlon - 10. 
     	    zlonur = curlon + 10. 
     	    zlatll = curlat - 10. 
     	    zlatur = curlat + 10. 
	    contur = .false.
	END IF
C
C*	Put a 5 degree margin around edge of plot.
C
	zlatll = zlatll - 5.
	zlatur = zlatur + 5.
	zlonur = zlonur + 5.
	zlonll = zlonll - 5.
C
	IF ( zlonll .lt. RLONLL ) zlonll = RLONLL
	IF ( zlonur .gt. RLONUR ) zlonur = RLONUR
	IF ( zlatll .lt. RLATLL ) zlatll = RLATLL
	IF ( zlatur .gt. RLATUR ) zlatur = RLATUR
C
C*	Check whether there is contourable data at 58 degrees or above.
C*	IF so, expand the plot area to make room for the legend.
C
	IF ( ier1 .eq. 1 )  zlatur = 63.
C
C*	Set margins, projection and graph area for plot.
C
	CALL GSMMGN ( 0., 0., 0., 0., ier )
	CALL GSMMAP ( 'MER', zlatll, zlonll, zlatur, zlonur, ier )
C
C*      Fill out the margin areas with the rest of the map.
C
        CALL GQBND ( 'N', xmnlat, xmnlon, xmxlat, xmxlon, ier )
        CALL GTRANS ( 'N', 'M', 1, xmnlat, xmnlon, rmnlat, rmnlon, ier )
        CALL GTRANS ( 'N', 'M', 1, xmxlat, xmxlon, rmxlat, rmxlon, ier )
        CALL GSMMAP ('MER', rmnlat, rmnlon, rmxlat, rmxlon, ier )
        CALL GQBND ( 'M', zlatll, zlonll, zlatur, zlonur, ier )
C	
C*	Draw boundary fill.
C
   	coltag = 'p_water_mask'
        CALL ST_LSTR ( coltag, lens, ier )
        CALL GH_COLR ( coltag(:lens), 24, ier)
        CALL GQCOLR ( icolr, ier)
        CALL ST_INCH ( icolr, cmap, ier )
        CALL ST_LSTR ( cmap, lens, ier )
        bgsea =  'bg/' // cmap(:lens) // '//1' 
        CALL GG_BND ( bgsea, ier )
        IF ( ier .lt. 0 ) mapok = .false.
C
        coltag = 'p_land_mask'
        CALL ST_LSTR ( coltag, lens, ier )
        CALL GH_COLR ( coltag(:lens), 29, ier)
        CALL GQCOLR ( jcolr, ier)
        CALL ST_INCH ( jcolr, cmap, ier )
        CALL ST_LSTR ( cmap, lens, ier )
C
        bgland = 'hcn_bnds/' // cmap(:lens)
        CALL GG_BND ( bgland, ier )
        IF ( ier .lt. 0 ) mapok = .false.
C
 	coltag = 'p_water_mask'
        CALL ST_LSTR ( coltag, lens, ier )
        CALL GH_COLR ( coltag(:lens), 24, ier)
        CALL GQCOLR ( icolr, ier)
        CALL ST_INCH ( icolr, cmap, ier )
        CALL ST_LSTR ( cmap, lens, ier )
        bglake = 'lakes/' // cmap(:lens)
        CALL GG_BND ( bglake, ier )
        IF ( ier .lt. 0 ) mapok = .false.
C
	IF ( ( contur ) .and. ( mapok ) ) THEN
C
C*	    Set contour attributes.
C
	    CALL GSCNTR ( 0, -1, -1, -1, ier )
C
C*	    Define contour color fill levels.
C
	    nflvl = 3
	    flvl ( 1 ) = 10.
	    flvl ( 2 ) = 20.
	    flvl ( 3 ) = 50.
C
C*	    Get the colors and labels.
C
 	    ifcolr ( 1 ) = 0
	    iflabl ( 1 ) = 1
	    ifltyp ( 1 ) = 1
	    DO ii = 2, 4
 	        coltag = prbclr (ii) 
                CALL ST_LSTR ( coltag, lens, ier )
                CALL GH_COLR ( coltag(:lens), ipclr(ii), ier)
                CALL GQCOLR (ifcolr(ii), ier )
	        iflabl ( ii ) = 1
	        ifltyp ( ii ) = 1
            END DO
C
	    ioffx = 0
	    ioffy = 0
	    iskip = 0
C
C*	    Draw contours.  Data extends from RLATLL to RLATUR and from 
C*	    RLONLL to RLONUR.  Grid is oriented such that (1,1) is at 
C*	    RLATLL, RLONLL and (96,60) is at RLATUR, RLONUR. 
C
	    CALL GCFILL  ( imax, jmax, grid, ioffx, ioffy, iskip,
     +		           nflvl, flvl, ifcolr, iflabl, ifltyp, iret )
	    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'GPTPC', -9, ' ', ier )
C
C*	    Plot an oval at the center, 50 nm to the left and 75 nm to
C*	    the right of the current direction of motion.  If there is
C*	    no direction, plot a circle of 62.5 nm radius at the center.
C
            coltag = 'p_prob_eq_100'
            CALL ST_LSTR ( coltag, lens, ier )
            CALL GH_COLR ( coltag(:lens), 2, ier)
            CALL GQCOLR ( ilcolr(4), ier)
C
	    inum = 0
	    CALL GSFILL ( 1.5, 1, ier )
	    CALL ST_NUMB ( dirmv, move, ier )
	    IF ( ier .eq. 0 ) THEN
C
C*		Loop to get 100 percent probability, extending 50 nm to
C*      	left and 75 nm to right of the direction of motion.
C*      	This section of code was adapted from routine GH_SWLN.
C
       		angle ( 1 ) = 360. - move
       		angle ( 2 ) = angle ( 1 ) + 90.
		angle ( 3 ) = angle ( 2 ) + 90.
		angle ( 4 ) = angle ( 3 ) + 90.
C
		ddist ( 1 ) = RDIST
		ddist ( 2 ) = 75.
		ddist ( 3 ) = ddist ( 1 )
		ddist ( 4 ) = 50.
		ddist ( 5 ) = ddist ( 1 )
C
C*	 	Loop through each quadrant of a track point.
C
                DO jj = 1, 4
                    DO ideg = 0, 90
                        inum = inum + 1
			theta = FLOAT ( ideg ) 
			rtheta = DTR * theta
                        rad = COS (rtheta) * COS (rtheta) * ddist (jj) 
     +                      + SIN (rtheta) * SIN (rtheta) * ddist (jj+1)
C
			rtheta = DTR * ( theta + 270.0 + angle ( jj ) )
                        y1 = rad * SIN ( rtheta ) / 60.0
                        x1 = (-rad) * COS ( rtheta ) / 60.0 
     +                       / COS ( DTR * curlat )
C
                        xlat ( inum ) = curlat + y1
                        xlon ( inum ) = curlon - x1
                    END DO
                END DO
C
	      ELSE
C
C*		Get the 100 percent probability as a circle of radius
C* 		62.5 nm.
C
	        dist = PR_HGNM ( RDIST )
	        incr = 10
	        DO ideg = 0, 360, incr
	            inum = inum + 1
	            bear = FLOAT ( ideg )
	            CALL CLO_DLTLN ( curlat, curlon, dist, bear,
     +			             xlat (inum), xlon (inum), ier )
	        END DO
	    END IF
	    CALL GFILL ( 'M', inum, xlat, xlon, ier )
	END IF    
C
C*	Draw map and lat/lon lines.
C
        IF ( mapok ) THEN
            CALL ST_INCH ( iblk, cblk, ier)
            CALL ST_LSTR ( cblk, lens, ier)
            mapstr = cblk(:lens) // '/1/1'
	    CALL IP_SVAR ( '$MAPFIL=hipowo.cia', ier )
            CALL GG_MAP  ( mapstr, ier )
            CALL GG_LTLN ( latlon, ier )
	    CALL GG_SCAL ( mscale, ier )
        END IF
C
	CALL GSCOLR ( 1, ier )
        CALL ST_LCUC ( strnam, strnam, ier )
C
        IF ( ( contur ) .and. ( mapok ) ) THEN
C
C*	    Plot the legend.
C
	    pllat = zlatur
	    pllon = ( zlonll + zlonur ) * .5
	    DO ii = 1, 3
	        ilcolr ( ii ) = ifcolr ( ii + 1 )
	    END DO
	    CALL GH_SPLB ( strnam, dattim, advno, curlon, pllat, pllon,
     +		           contur, ilcolr, ier )
          ELSE
C
C*          Plot text if there is no strike probability.
C
            CALL ST_LSTR ( strnam, lenn, ier )
            CALL ST_LSTR ( advno, lena, ier )
            noprob = 'NO STRIKE PROBABILITIES AVAILABLE FOR ' //
     +		     strnam (:lenn) // ' ADVISORY ' // advno (:lena)
            CALL ST_LSTR ( noprob, lens, ier )
	    CALL GSCOLR ( iwht, ier )
            CALL GSTEXT ( 22, 2, 1.2, 1, 111, 1, 2, ier )
 	    CALL GTEXT ( 'N', .5, .4, noprob (:lens), 0.0, 0, 0,
     +                   ier )
	END IF
C
C*	Plot NOAA logo.
C
	CALL GLOGO ( 'N', .90, .07, 2.5, 2, 1, ier )
C
C*	Plot NWS logo.
C
	CALL GLOGO ( 'N', .11, .07, 2.5, 2, 2, ier )
C
C*	Reset the saved attributes.
C
	CALL GSGPRJ ( cproj,  angle1, angle2, angle3, imx, imy,
     +		      dlatll, dlonll, dlatur, dlonur, iret )
C
        CALL GH_REST ( ier ) 
	iret = 0
C*
	RETURN
	END
