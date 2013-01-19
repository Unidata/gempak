	SUBROUTINE GH_KGIP ( tlat, tlon, rmnlat, rmnlon, rmxlat, rmxlon,
     +                       bxlgth, slnlat, slnlon, numln, bxwdth, 
     +                       ddev, curlat, curlon, 
     +                       nabk, abklat, abklon,
     +                       cclat, cclon, poslb, iret )
C************************************************************************
C* GH_KGIP								*
C*									*
C* This subroutine determines the legend box position for the tropical  *
C* cyclone watch/warn graphic.  It checks the possible legend box corner*
C* positions in the order of LR, UR, LL, UL.  It first performs a crude *
C* check against a box which encloses the track error area, including   *
C* a surrounding margin.  If this check fails, it checks against the    *
C* track error area itself.  If the legend box and track error box/track*
C* error cone do not overlap, the check for overlap of brekapoints and  *
C* legend box is performed.  If there is no overlap the check of overlap*
C* of NOAA logo and track error box is performed.  The position will 	*
C* default to the LR corner if an overlap is found for all four corner  *
C* positions.								*
C*									*
C* GH_KGIP ( TLAT, TLON, RMNLAT, RMNLON, RMXLAT, RMXLON, BXLGTH, 	*
C*           SLNLAT, SLNLON, NUMLN, BXWDTH, DDEV, CURLAT, CURLON,       *
C*	     NABK, ABKLAT, ABKLON, CCLAT, CCLON, POSLB, IRET )		*
C*									*
C* Input Parameters:							*
C*	TLAT (*)	REAL		Track error box area - lats.	*
C*	TLON (*)	REAL		Track error box area - lons.	*
C*	RMNLAT 		REAL		Minumum garea latitude		*
C*	RMNLON 		REAL		Mininum garea longitude		*
C*	RMXLAT 		REAL		Maximum garea latitude		*
C*	RMXLON 		REAL		Maximum garea longitude		*
C*	BXLGTH		REAL		Legend box length (norm. coords)*
C*	SLNLAT (NUMLN) 	REAL		Track error area - lats.	*
C*	SLNLON (NUMLN)	REAL		Track error area - longs.	*
C*	NUMLN		INTEGER		Number of track error points	*
C*	BXWDTH		REAL		Legend box width (norm. coords) *
C*	DDEV		CHAR*		Device driver			*
C*	CURLAT		REAL		Current latitude                *
C*	CURLON		REAL		Current longitude               *
C*	NABK		INTEGER		Number of all breakpoints	*
C*	ABKLAT (NABK) 	REAL		All breakpoints - lats.		*
C*	ABKLON (NABK)	REAL		All breakpoints - longs.	*
C*									*
C* Output Parameters:							*
C*	CCLAT		REAL 		Anchor corner lat. (norm.)      *
C*	CCLON		REAL 		Anchor corner long. (norm.)     *
C*	POSLB		CHAR 		Flag for legend position	*
C*	IRET		INTEGER		Return code			*
C*					    0 = normal return           *
C*					   -1 = unable to place box     *
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01						*
C* A. Hardy/GSC		 6/01	Added 2 more checks for legend  	*
C* A. Hardy/SAIC	 8/01	Added PS check;incr. box size		*
C* D. Kidwell/NCEP	10/01	Increased dims of xlat, ylon    	*
C* A. Hardy/SAIC	10/01	Added current lat/lon			*
C* D. Kidwell/NCEP	 4/02	Cleaned up, added iret = -1     	*
C* D. Kidwell/NCEP	 4/02	Added check around current loc. 	*
C* D. Kidwell/NCEP	 4/04	Added check for logo overlap    	*
C* S. Gilbert/NCEP	 7/05	Corrected position of label box 	*
C* m.gamazaychikov/SAIC	09/08	CSC - added nabk, abklat, abklon,	*
C*                          	cleaned up and added check for overlap	*
C*                          	between breakpoints and legend box	*
C************************************************************************
	REAL  		tlat(*), tlon(*), slnlat(*), slnlon(*),
     +                  abklat(*), abklon(*)
        CHARACTER*(*)   ddev
        CHARACTER       poslb 
C*
        REAL            xlat(500), ylon(500), xpoly(5), ypoly(5),
     +			bxlat (3), bxlon (3), xlatbx (5), ylonbx (5),
     +			xlogo (9), ylogob (9), ylogot (9)
        CHARACTER       sysn*2, svlb, fliplb
 	LOGICAL         done, good,
     +                  curchck, curgood, logchck, loggood,
     +                  boxchck, conchck, bkpchck,
     +                  boxgood, congood, bkpgood
C
C*	The following are the normalized coordinates for an octagon 
C*	surrounding the NOAA logo.
C
      DATA		xlogo  / .982, .967, .930, .893, .878, .893,
     +				 .930, .967, .982 /
      DATA		ylogob / .070, .107, .122, .107, .070, .033,
     +				 .018, .033, .070 /
      DATA		ylogot / .720, .757, .772, .757, .720, .683, 
     +				 .668, .683, .720 /
C------------------------------------------------------------------------
        iret  = 0 
	poslb = 'B'
	done  = .false.
	bkpchck  = .false.
	boxchck  = .false.
	conchck  = .false.
	curchck  = .false.
	logchck  = .false.
	bkpgood  = .false.
	boxgood  = .false.
	congood  = .false.
	curgood  = .false.
	loggood  = .false.
        ipoly = 5
        sysn  = 'N'
        CALL ST_NULL ( sysn, sysn, lens, ier )
C 
C*      Get normalized coordinates for both box around track error area
C*	and for track error area itself.
C
	CALL GTRANS ( 'M', 'N', ipoly, tlat, tlon, xlatbx, ylonbx, ier )
	CALL GTRANS ( 'M', 'N', numln, slnlat, slnlon, xlat, ylon, ier )
        xmax = rmxlat
        xmin = rmnlat
        ymax = rmxlon
        ymin = rmnlon
C
C*      Find the suitable placement of legend box.
C
        ii    = 1
        isave = 0
        bxcnr = bxwdth - .19
        bxcnr = bxwdth + .01
        DO WHILE ( ( .not. done ) .and. ( ii .le. 4 ) )
             IF ( ii .eq. 1 ) THEN
C
C*              Lower right
C
                poslb = 'B'
                xpoly (1) = xmax - bxcnr 
                ypoly (1) = ymin + bxlgth + .03
                IF  ( ddev(1:2) .eq. 'PS' ) xpoly(1)=xmax-bxcnr+.02
             ELSE IF ( ii .eq. 2 ) THEN
C
C*              Upper right
C
                poslb = 'T'
                xpoly (1) = xmax - bxcnr
                ypoly (1) = ymax - .03
                IF  ( ddev(1:2) .eq. 'PS' )  THEN
                    xpoly (1) = xmax - bxcnr +.02
                    ypoly (1) = ymax - .05
                END IF
             ELSE IF ( ii .eq. 3 ) THEN
C
C*              Lower left
C
                poslb = 'B'
                xpoly (1) = .045
                ypoly (1) = ymin + bxlgth + .03
             ELSE 
C
C*              Upper left
C
                poslb = 'T'
                xpoly (1) = .045 
                ypoly (1) = ymax - .03
                IF  ( ddev(1:2) .eq. 'PS' ) ypoly (1) = ymax - .05
             END IF
C
C*           Set up legend box polygon.
C
             xpoly (2) = xpoly(1)
             ypoly (2) = ypoly(1) - bxlgth
             xpoly (3) = xpoly(2) + bxwdth
             ypoly (3) = ypoly(2) 
             xpoly (4) = xpoly(3)
             ypoly (4) = ypoly(1) 
             xpoly (5) = xpoly(1)
             ypoly (5) = ypoly(1) 
C
C*           Overlap of error box and legend box check
C
             IF ( .not. boxchck ) THEN
C
C*               Check the error box overlap with the proposed
C*               legend box position
C
                 CALL CGR_POLYINT ( sysn(:lens), 5, xpoly, ypoly, 
     +                              sysn(:lens), ipoly, xlatbx, 
     +                              ylonbx, inout, ier )
                 IF ( inout .eq. 0 ) THEN
                    boxgood = .true.
                    cclat = ypoly(1) 
                    cclon = xpoly(1) 
                    isave = ii
                    svlat = ypoly (1)
                    svlon = xpoly (1)
                    svlb  = poslb (1:1)
                 END IF
                 boxchck = .true.
             END IF
C
C*           Overlap of error cone and legend box check
C
             IF ( .not. conchck .and. .not. boxgood ) THEN
C
C                Check the current storm location relative to the
C*               proposed legend box position.
C
                 IF ( .not. curchck ) THEN
                    CALL GTRANS ( 'N', 'M', 3, xpoly, ypoly, bxlat,
     +                            bxlon, ier )
                    bxrgt = bxlon ( 3 ) + 3.
                    bxlft = bxlon ( 1 ) - 3.
                    bxtop = bxlat ( 1 ) + 3.
                    bxbot = bxlat ( 2 ) - 3.
                    curgood  = .false.
                    IF ( ii .eq. 1 ) THEN
                       IF ( ( curlon .le. bxlft ) .or.
     +                      ( curlat .ge. bxtop ) )  curgood = .true.
                    ELSE IF ( ii .eq. 2 ) THEN
                       IF ( ( curlon .le. bxlft ) .or.
     +                      ( curlat .le. bxbot ) )  curgood = .true.
                    ELSE IF ( ii .eq. 3 ) THEN
                       IF ( ( curlon .ge. bxrgt ) .or.
     +                      ( curlat .ge. bxtop ) )  curgood = .true.
                    ELSE
                       IF ( ( curlon .ge. bxrgt ) .or.
     +                      ( curlat .le. bxbot ) )  curgood = .true.
                    END IF
                 END IF
                 curchck  = .true.
C
C*               Check the error cone overlap with the proposed 
C*               legend box position
C
                 CALL CGR_POLYINT ( sysn(:lens), 5, xpoly, ypoly,
     +                              sysn(:lens), numln, xlat, ylon,
     +                              inout, ier )
                 IF ( ( inout .eq. 0 ) .and. curgood ) THEN
                    cclat = ypoly(1) 
                    cclon = xpoly(1) 
                    conchck = .true.
                    congood = .true.
                    isave = ii
                    svlat = ypoly (1)
                    svlon = xpoly (1)
                    svlb  = poslb (1:1)
                 ELSE
C
C*                    Set the conchck to false to try to place
C*                    legend in a different corner
C
                      conchck = .false.
                 END IF
             END IF
C
C*           Overlap of breakpoints and legend box check
C
             IF ( .not. bkpchck .and. ( boxgood .or. congood ) ) THEN
                 ibk = 1
                 DO WHILE ( (ibk .le. nabk) .and. (inout .eq. 0) )
                    CALL GTRANS ( 'M', 'N', 1, abklat(ibk), abklon(ibk),
     +                             xbk, ybk, ier )
                    CALL CGR_INPOLY ( sysn(:lens), 1, xbk,   ybk,
     +                                sysn(:lens), 5, xpoly, ypoly,
     +                                inout, ier)
                    ibk = ibk + 1
                 END DO
                 bkpchck = .true.
                 IF ( inout .eq. 0 ) bkpgood = .true.
             END IF 
C
C*           Overlap of NOAA Logo and legend box check
C
             IF ( .not. logchck .and. ( boxgood .or. congood ) 
     +                          .and.   bkpgood                ) THEN
C
C*               Check the logo position relative to the
C*               proposed legend box position.
C
                 logchck = .true.
                 IF ( poslb .eq. 'B' ) THEN
                    CALL CGR_POLYINT ( sysn(:lens), 9, xlogo, ylogot,
     +                                 sysn(:lens), numln, xlat, ylon,
     +                                 inout, ier )
                 ELSE
                    CALL CGR_POLYINT ( sysn(:lens), 9, xlogo, ylogob,
     +                                 sysn(:lens), numln, xlat, ylon,
     +                                 inout, ier )
                 END IF
                 IF ( inout .eq. 0 ) THEN
                    loggood = .true. 
                 ELSE
                 END IF
             END IF

C
C*           Go to the next corner in the progression
C*           unless there is no overlap between either:
C*           - error box  and legend box
C*           - error cone and legend box
C*                     AND
C*           - breakpoints and legend box
C*                     AND
C*           - NOAA logo and legend box
C 
             ii = ii + 1
             IF ( ( boxgood .or. congood ) .and. bkpgood 
     +                .and. loggood ) THEN
                  done = .true. 
             ELSE
                  bkpchck = .false.
                  boxchck = .false.
                  conchck = .false.
                  curchck = .false.
                  logchck = .false.
             END IF
        END DO 
C
        IF ( .not. done ) THEN
C
C*          An overlap or near overlap is going to occur anywhere.  In
C*	    case this is the final attempt to place the box, default to
C*	    LR corner for an overlap, or to previously saved corner 
C*	    (with current point near box).
C
	    iret  = -1
	    IF ( isave .le. 1 ) THEN
	        poslb = 'B'
                cclat = ymin + bxlgth + .03
                cclon = xmax - bxcnr
                IF ( ddev(1:2) .eq. 'PS' ) cclon = cclon +.02
	      ELSE
		poslb = svlb
		cclat = svlat
		cclon = svlon
                IF ( ddev(1:2) .eq. 'PS' ) THEN
		    IF ( isave .eq. 2 ) THEN
		        cclat = cclat - .02
			cclon = cclon + .02
		      ELSE IF ( isave .eq. 4 ) THEN
			cclat = cclat - .02
		    END IF
		END IF
	    END IF
        END IF
C*
	RETURN
	END
