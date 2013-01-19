	SUBROUTINE GDRGRD  ( dellat, dellon, lblfrq, blat, blon,
     +                       ifrmat, iret )
C************************************************************************
C* GDRGRD								*
C* 									*
C* This subroutine draws a uniform latitude/longitude grid.  The map 	*
C* projection must be defined before GDRGRD is called. The current 	*
C* color, line and text attributes will be used.  The latitude and 	*
C* longitude intervals are given in degrees.				*
C* 									*
C* GDRGRD  ( DELLAT, DELLON, LBLFRQ, BLAT, BLON, IFRMAT, IRET )		*
C* 									*
C* Input parameters:							*
C*	DELLAT		REAL  		Latitude interval 		*
C*	DELLON		REAL		Longitude interval 		*
C*	LBLFRQ (2)	INTEGER		Label frequency			*
C*				  	  0 = no grid labels		*
C*				   	  1 = every grid line		*
C*				   	  2 = every other grid line	*
C*				   	  n = every n-th line		*
C*	BLAT		REAL		Longitude label locations	*
C*	BLON		REAL		Latitiude label locations	*
C*	IFRMAT		INTEGER		Lat/lon format			*
C*					  1 = +/- value			*
C*					  2 = N,S,E,W added, no '-'	*
C*					  Defaults to 1 if not set to 2 *
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 3/82						*
C* M. Vilardo/RDS	 6/84	GEMPLT Version 3.0			*
C* M. Vilardo/RDS	 7/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 8/86	Added grid line annotation		*
C* M. desJardins/GSFC	 5/88	Eliminated call to GFLUSH		*
C* K. F. Brill/GSC	 4/89   Fixed labelling; extended parallels	*
C* K. Brill/GSC          1/90   Use PRNLON for longitude labels		*
C* K. Brill/GSC          1/90   Check for cut for conic projections	*
C* K. Brill/GSC          1/90   Label meridians at north for SH maps	*
C* K. Brill/NMC          8/90   Fix parallels gap; remove left meridian *
C* K. Brill/NMC          8/90   Fix labelling problems			*
C* K. Brill/NMC          9/90   Increase resolution from 2 to .25 deg	*
C* K. Brill/NMC		 1/91   Decimal lon>100; remove bottom lat line;*
C*                              shift lat labels.			*
C* J. Whistler/SSAI	 8/91	Changed LBFREQ to a 2-D array		*
C* S. Jacobs/EAI	 1/93	Added check for South Pole; Cleaned up	*
C* S. Jacobs/EAI	10/93	Changed CLABEL --> GR_LABL		*
C* K. Brill/NMC		 1/94	Check to stop the DO WHILE (.not. done) *
C*				loops by checking too big lat or lon	*
C* S. Jacobs/EAI	 2/94	Added check for satellite projections	*
C* J. Cowie/COMET	 3/95   Added MCGOES and MCGVAR to sat proj	*
C* S. Jacobs/NMC	 6/95	Check for missing values from GTRANS	*
C*				for computing the line vertices		*
C* M. Linda/GSC		12/95	Removed navtyp = NPGS 			*
C* L. Sager/NCEP 	12/95   Fixed error in lat/lon plotting		*
C* K. Brill/EMC		 3/96	Changes for general rotated projections *
C* S. Jacobs/NCEP	 3/97	Added check for UTF and VG drivers	*
C* T. Lee/GSC		 9/97	Fixed typo, ILLON -> IILON		*
C* A. Hardy/GSC          9/98   Added check for RBK driver              *
C* S. Danz/AWC           5/99   Added check for date-line crossover     *
C* T. Piper/GSC		12/99	Slight change to dmin calculation	*
C* A. Hardy/GSC		12/00   Added lat/lon label locations, format	*
C* J. Cowie/COMET	12/00	Added MCRADR projection type		*
C* S. Jacobs/NCEP	 2/01	Changed label for 0 lat & 0,180 lon	*
C* S. Chiswell/Unidata	 2/02	Added MCMSAT, MCGMSX, MCMOLL projection	*
C* T. Lee/SAIC		 4/03	Fixed latitude plot when pole is in view*
C* S. Jacobs/NCEP	 9/05	Check both X & Y for lat/lon label loc	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
	INCLUDE 	'DEVCHR.CMN'
C*
	PARAMETER	( MXPTS = 1442 )
C*
	INTEGER		lblfrq (*)
C*
	REAL		alat ( MXPTS ), alon ( MXPTS )
	REAL		alatl ( MXPTS ), alonl ( MXPTS )
	REAL		xp (2), yp (2)
	CHARACTER 	string*20, proj*4, drct*1
	CHARACTER	navtyp*8, imgnam*80
	LOGICAL		done, hvnpol, hvspol, notcut, needdl, drawit
	LOGICAL		topole
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If the device is UTF, VG or RBK, do not draw the lat/lon lines.
C
	IF  ( ( ddev .eq. 'UTF' ) .or. ( ddev .eq. 'VG' ) .or.
     +        ( ddev .eq. 'RBK' ) )  RETURN
C*
	needdl = .true.
	imxpm1 = MXPTS - 1
C
C*	Check for map mode.
C
	igmode = 1
	IF ( igmode .ne. 1 ) THEN
C*
	    iret = nimode
C*
	ELSE
C
C*	    Query map projection setup to get center longitude.
C
	    CALL GQMPRJ ( proj, angl1, clon, angl3,
     +			  dlatmn, dlonmn, dlatmx, dlonmx, iret )
C
C*	    Check for satellite navigation.
C
	    IF  ( iret .ne. NORMAL )  THEN
		CALL GQSATN ( navtyp, imgnam, ier )
		IF  ( ( navtyp .ne. 'MCIDAS' ) .and.
     +			( navtyp .ne. 'MCGOES' ) .and.
     +			( navtyp .ne. 'MCGVAR' ) .and.
     +			( navtyp .ne. 'MCMSAT' ) .and.
     +			( navtyp .ne. 'MCGMSX' ) .and.
     +			( navtyp .ne. 'MCMOLL' ) .and.
     +			( navtyp .ne. 'MCRADR' ) ) RETURN
	    END IF
C
C*	    Query the boundaries in map coordinates.
C*
	    CALL GQBND ( 'M', rlatmn, rlonmn, dlatmx, dlonmx, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
C
C*	    Check  for possible wrap around the date line
C
	    IF  ( dlonmx .lt. rlonmn )  dlonmx = 360 + dlonmx
C
C*	    Query plot region.
C
	    CALL GQBND ( 'P', vlp, vbp, vrp, vtp, iret)
	    IF  ( iret .ne. NORMAL )  RETURN
C
C*          Find out if poles are visible and find fractional position.
C
	    CALL GPTVIS ( 'M', 1, 90.0, 0.0, hvnpol, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
	    IF  ( hvnpol )  THEN
		CALL GTRANS ( 'M', 'N', 1, 90.0, 0.0, xnp, ynp, ier )
		fractn = ( ynp - vbp ) / ( vtp - vbp )
	    END IF
	    CALL GPTVIS ( 'M', 1, -90.0, 0.0, hvspol, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
	    IF  ( hvspol )  THEN
		CALL GTRANS ( 'M', 'N', 1, -90.0, 0.0, xnp, ynp, ier )
		frctn2 = ( ynp - vbp ) / ( vtp - vbp )
	    END IF
C
C*	    Query symbol sizes.
C
	    CALL GQSYSZ ( wm, hm, wt, ht, wb, hb, iret )
	    IF  ( iret .ne. NORMAL )  RETURN
C
C*	    Compute tolerances for determining endpts of
C*	    lat & lon lines. (Leave space for labels.)
C
	    tollat = .5 * ht * ( dlatmx - rlatmn ) / ( vtp - vbp )
	    tollon = .5 * wt * ( dlonmx - rlonmn ) / ( vrp - vlp )
C
C*	    Calculate the starting, ending and number of parallels.
C
	    IF  ( rlatmn .lt. 0.0 .or. MOD(rlatmn,dellat) .eq. 0 )  THEN
		slat = INT ( rlatmn / dellat ) * dellat
	    ELSE
		slat = ( INT ( rlatmn / dellat ) + 1 ) * dellat
	    END IF
	    IF  ( dlatmx .gt. 0.0 .or. MOD(dlatmx,dellat) .eq. 0 )  THEN
		elat = INT ( dlatmx / dellat ) * dellat
	    ELSE
		elat = ( INT ( dlatmx / dellat ) - 1 ) * dellat
	    END IF
	    nlat = ( elat - slat ) / dellat
C
C*	    Determine where to start the labeling so that the first
C*          label is a whole number if possible.
C
	    IF  ( lblfrq(1) .gt. 1 )  THEN
		done = .false.
		iilat = 0
		DO  WHILE ( .not. done .and. iilat .gt. -11 )
		    iilat = iilat - 1 
		    rqlat = slat - FLOAT ( iilat + 1 ) * dellat
		    tstlt = FLOAT ( INT ( rqlat ) )
		    IF  ( ABS(tstlt-rqlat) .lt. 10e-5 )  done = .true.
		END DO
		IF ( IABS (iilat) .gt. lblfrq(1) ) iilat = -1 
	    ELSE
		iilat = -1
	    END IF
C
C*	    Loop thru latitude range.
C
	    IF  ( slat .le. rlatmn )  THEN
		ist01 = 1
	    ELSE
		ist01 = 0
	    END IF
	    DO  ilat = ist01, nlat
		iilat = iilat + 1
		rqlat = slat + ilat * dellat
		dlonmn = rlonmn
		IF  ( lblfrq(1) .gt. 0 )  THEN
		    dlt = rqlat
		    done = .false.
		    DO  WHILE (.not. done)
C
C*			Find location of west lon and latitude.
C
			dln = dlonmn
			CALL GTRANS ( 'M', 'N', 1, dlt, dln,
     +				      dmnln, dnly, ier )
C
C*	                Find min, calc location and compare.
C
			IF ( ERMISS ( dmnln ) ) THEN
			    dmin = vlp
			ELSE
			    dmin = dmnln - 4. * wt
			END IF
			IF  ( dmin .lt. vlp )  THEN
			    IF  ( .not. hvnpol .and. .not. hvspol ) THEN
				dlonmn = dlonmn + tollon
			      ELSE
				done = .true.
			    END IF
C
			    IF ( dlonmn .gt. 360. ) THEN
				dlonmn = 360.
			        done = .true.
			    END IF
			ELSE
			    done = .true.
			END IF
		    END DO
		END IF
C
C*	        Calculate the number of line segemets required to draw
C*		a parallel. Parallels must be split up into line
C*		segemnts of .25 degrees longitude to be sure that any
C*		curvature in the parallel is correctly displayed.
C
		nlons = ABS(   dlonmx - dlonmn ) / .250 + 1.0 
		dlons = ABS( ( dlonmx - dlonmn ) / FLOAT( nlons - 1 ) )
C
C*	        Loop thru .25 degree longitude segments.
C*		Calculate coordinates.
C
		IF ( nlons .gt. imxpm1 ) nlons = imxpm1
		DO  ilons = 0, nlons
		    alat( ilons + 1 ) = rqlat
		    alon( ilons + 1 ) = dlonmn + ilons * dlons
		END DO
C
C*		Move latitudes and longitudes into plotting arrays.
C
		CALL PRNLON ( nlons, alon, ier )
		DO  i = 1, nlons
		    alatl( i ) = alat( i )
		    alonl( i ) = alon( i )
		    IF ( i .ne. 1 ) THEN
			IF  ( alonl (i)   .lt. 0.0 .and.
     +                    alonl (i-1) .gt. 0.0 )  THEN
			    alonl (i-1) = 180.
			    alonl (i) = -180.
			END IF
		    END IF
		END DO
C
C*		Plot line for parallel.
C
		IF  ( mclass .ne. 3 )  THEN
C
C*		    Line will be continuous.
C
		    CALL GLINE  ( 'M', nlons, alatl, alonl, ierr )
		ELSE
C
C*                  If the line crosses the conic cut, it must be
C*                  done in pieces.
C
		    cutang = clon + 180.
		    CALL PRNLON ( 1, cutang, ier )
		    istrt = 1
		    indx  = 1
		    DO  WHILE ( indx .lt. nlons )
			notcut = .true.
			icnt = 0
			DO  WHILE ( notcut )
			    icnt = icnt + 1
			    inm1 = indx
			    indx = indx + 1
			    pxll = alonl (inm1) * alonl (indx)
			    IF  ( alonl (indx) .eq. cutang   .or.
     +			          ( alonl (inm1) .gt. cutang .and.
     +                              alonl (indx) .lt. cutang ) .or.
     +                            ( alonl (inm1) .lt. cutang .and.
     +                              alonl (indx) .gt. cutang ) .or.
     +                            ( cutang .eq. 180. .and.
     +				    pxll .lt. 0. )            .or.
     +                            ( cutang .eq. -180. .and.
     +                              pxll .lt. 0. )         )  THEN
				IF  ( alonl (istrt) .eq. cutang )  THEN
				    istrt = istrt + 1
				    icnt  = icnt - 1
				END IF
				IF  ( alonl(istrt+icnt-1) .eq. cutang )
     +				    icnt = icnt - 1
				IF  ( icnt .gt. 1 )
     +				    CALL GLINE ( 'M', icnt,
     +						 alatl(istrt),
     +						 alonl (istrt), ierr )
C*
				notcut = .false.
				istrt  = indx
			    ELSE IF  ( indx .eq. nlons )  THEN
				icnt = icnt + 1
				IF  ( alonl (istrt) .eq. cutang )  THEN
				    istrt = istrt + 1
				    icnt  = icnt - 1
				END IF
				IF  ( alonl(istrt+icnt-1) .eq. cutang )
     +				    icnt = icnt - 1
				IF  ( icnt .gt. 1 )
     +				    CALL GLINE ( 'M', icnt,
     +						 alatl (istrt),
     +						 alonl (istrt), ierr )
				notcut = .false.
			    END IF
			END DO
		    END DO                             
		END IF
C
C*		If label is requested for this parallel begin label 
C*		processing.
C
		IF  ( lblfrq(1) .gt. 0 .and. iilat .ge. 0 .and.
     +		      MOD ( iilat, lblfrq(1) ) .eq. 0 )  THEN
		    al =  slat + ilat * dellat 
C
C*		    If the pole is less than six tenths of the way
C*		    between the bottom and top of the plot, then the
C*		    labelling is done along the central longitude on
C*		    the opposite side of the pole.
C
 		    IF (  ERMISS ( blon )  ) THEN
		        IF  ( hvnpol .and. mclass .ne. 1 )  THEN
			    IF  ( fractn .lt. 0.6 )  THEN
			        a180 = 180.
			      ELSE
			        a180 = 0.0
			    END IF
			    IF  ( clon .lt. 0. )  THEN
			        bot = clon + a180
			      ELSE
			        bot = clon - a180
			    END IF
		          ELSE IF  ( hvspol .and. mclass .ne. 1 )  THEN
			    IF  ( frctn2 .lt. 0.6 )  THEN
			        a180 = 180.
			      ELSE
			        a180 = 0.0
			    END IF
			    IF  ( clon .lt. 0. )  THEN
			        bot = clon + a180
			      ELSE
			        bot = clon - a180
			    END IF
		          ELSE
			    bot = dlonmn
		        END IF
                      ELSE
                        bot = blon
                    END IF
C
C*                  Use 'N' and 'S' for encoding the label string.
C
                    IF ( ifrmat .eq. 2 ) THEN
                        IF ( al .lt. 0. ) THEN
			    al2 = ABS ( al )
                            drct = 'S'
        		  ELSE IF ( al .gt. 0. ) THEN
			    al2 = al
                            drct = 'N'
        		  ELSE
			    al2 = al
                            drct = ' '
                        END IF 
		        CALL GR_LABL ( al2, 0, 0, string, len, ier )
                        len = len + 1
                        string (len:len) = drct
		        ishft = (-len) * 2
                      ELSE
		        CALL GR_LABL ( al, 0, 0, string, len, ier )
		        ishft = (-len) * 2
		    END IF
C
C*		    Plot the label.
C		
		    CALL GTRANS ( 'M', 'V', 1, al, bot, x, y, ier )
		    IF  ( ( x .gt. vlp .and. x .lt. vrp ) .and.
     +			  ( y .gt. vbp .and. y .lt. vtp ) )
     +			CALL GTEXT ( 'V', x, y, string (1:len), 0.,
     +				     ishft, 0, ier )
		 END IF
	    END DO
C
C*	    Calculate the starting, ending and number of meridians.
C
	    IF  ( rlonmn .lt. 0.0 .or. MOD(rlonmn,dellon) .eq. 0 )  THEN
		slon = INT ( rlonmn / dellon ) * dellon
	    ELSE
		slon = ( INT ( rlonmn / dellon ) + 1 ) * dellon
	    END IF
	    IF  ( dlonmx .gt. 0.0 .or. MOD(dlonmx,dellon) .eq. 0 )  THEN
		elon = INT ( dlonmx / dellon ) * dellon
	    ELSE
		elon = ( INT ( dlonmx / dellon ) - 1 ) * dellon
	    END IF
	    nlon = ( elon - slon ) / dellon
C
C*	    Determine where to start the labeling so that the first
C*          label is a whole number if possible.
C
	    IF  ( lblfrq(2) .gt. 1 )  THEN
		done = .false.
		iilon = 0
		DO  WHILE ( .not. done .and. iilon .gt. -11 )
		    iilon = iilon - 1 
		    rqlon = slon - FLOAT ( iilon ) * dellon
		    tstln = FLOAT ( INT ( rqlon ) )
		    IF  ( ABS(tstln-rqlon) .lt. 10e-5 )  done = .true.
		END DO
		IF  ( IABS ( iilon ) .gt. lblfrq(2) )  iilon = 0
	    ELSE
		iilon = 0
	    END IF
	    test = ABS ( slon - rlonmn )
	    IF  ( test .lt. ( .5 * dellon ) )  THEN
		ist01 = 1
	    ELSE
		ist01 = 0
	    END IF
	    DO  ilon = ist01, nlon
		iilon  = iilon + 1
		rqlon  = slon + ilon * dellon
		dlatmn = rlatmn
		dmxlat = dlatmx
		IF  ( lblfrq(2) .gt. 0 )  THEN
		    done = .false.
		    dln = rqlon
		    DO  WHILE ( .not. done )
C
C*	                Find location in N coordinates of  
C*			longitude and South lat.
C
			dlt = dlatmn
			CALL GTRANS ( 'M', 'N', 1, dlt, dln,
     +				      wlx, dmnlt, iret )
C
C*			Find minimum, calc location and compare to
C*			view region.
C
			IF ( ERMISS ( dmnlt ) ) THEN
			    dmin = vbp
			ELSE
			    dmin = dmnlt -  ht
			END IF
			IF  ( dmin .lt. vbp )  THEN
				dlatmn = dlatmn + tollat
				IF ( dlatmn .gt. 90. ) done = .true.
			ELSE
				done = .true.
			END IF
		    END DO
		END IF
C
C*		Calculate the number of line segemets required to
C*		draw a meridian. Meridians must be split up into
C*		line segemnts of .25 degrees latitude to be sure
C*		that any curvature in the meridian is correctly
C*		displayed.
C
C*		Loop thru longitude range.
C*		Loop thru .25 degree latitude segments.
C
		altmx = dlatmx
		altmn = dlatmn
		qmer0 = angle1 + angle3
		IF  ( proj .eq. 'MCD' )  THEN
		    topole = .true.
		  ELSE
		    topole = ((qmer0 .eq. 0) .and. (mclass .eq. 1))
		END IF
	  	IF ( dlatmx .gt. 80. .and. .not. topole ) THEN
		    xp (1) = 90.
		    yp (1) = slon
		    xp (2) = 90.
		    yp (2) = elon
		    CALL GTRANS ( 'M', 'N', 2, xp, yp, xp, yp, ier )
		    dx = ABS ( xp (2) - xp (1) )
		    IF ( dx .lt. .01 ) THEN
		        nlats = ABS( 80.0 - altmn ) / .250 + 1.0 
		        dlats = ABS( ( 80.0 - altmn ) /
     +				FLOAT( nlats - 1 ) )
		    END IF
		    altmx = 80.0
		END IF
		IF ( rlatmn .lt. -80. .and. .not. topole ) THEN
		    xp (1) = -90.
		    yp (1) = slon
		    xp (2) = -90.
		    yp (2) = elon
		    CALL GTRANS ( 'M', 'N', 2, xp, yp, xp, yp, ier )
		    dx = ABS ( xp (2) - xp (1) )
		    IF ( dx .lt. .01 ) THEN
		        nlats = ABS( altmx - (-80.0) ) / .250 + 1.0 
		        dlats = ABS( ( altmx - (-80.0) ) /
     +				FLOAT( nlats - 1 ) )
		    END IF
		    altmn = -80.
		END IF
		IF ( ( .not. ( rlatmn .lt. -80. ) .and.
     +		       .not. ( dlatmx .gt. +80. ) ) .or.
     +			topole ) THEN
		    nlats = ABS( altmx - altmn ) / .250 + 1.0 
		    dlats = ABS( ( altmx - altmn ) /
     +				FLOAT( nlats - 1 ) )
		END IF
C
C*		Loop thru .25 degree latitude segments.
C*		Calculate coordinates.
C
		IF ( nlats .gt. imxpm1 ) nlats = imxpm1
		DO  ilats = 0, nlats
		    alat( ilats + 1 ) = altmn + ilats * dlats
		    alon( ilats + 1 ) = rqlon
		END DO
C
C*		Move latitudes and longitudes into plotting arrays.
C
		DO  i = 1, nlats
		    alatl( i ) = alat( i )
		    alonl( i ) = alon( i )
		END DO
C
C*	    	Plot line for meridian.
C
		CALL GLINE ( 'M', nlats, alatl, alonl, ierr )
C
C*		If label is requested for this meridian begin label 
C*		processing.
C
		at = rqlon
		CALL PRNLON ( 1, at, ier )
C
C*		Only label the dateline one time.
C
		naint = nint ( at )
		IF  ( naint .eq. 180 .or. naint .eq. -180 )  THEN
		    IF  ( needdl )  THEN
			drawit = .true.
		    ELSE
			drawit = .false.
		    END IF
		    needdl = .false.
		ELSE
		    drawit = .true.
		END IF
		IF  ( ( lblfrq(2) .gt. 0 ) .and. ( iilon .ge. 0 ) .and.
     +                ( MOD( iilon, lblfrq(2) ) .eq. 0 ) .and.
     +		      ( drawit ) )  THEN
                    IF ( ERMISS ( blat ) ) THEN
		        side = altmn
		        IF  ( side .lt. -89.9 )  side = altmx
                      ELSE
                        side = blat
                    END IF
C
C*                  Use 'E' and 'W' for encoding the label string.
C
		    IF ( ifrmat .eq. 2 ) THEN
                        IF ( at .lt. 0. ) THEN
                            at2 = ABS (at)
			    IF  ( at .eq. -180. )  THEN
				drct = ' '
			      ELSE
				drct = 'W'
			    END IF
                          ELSE IF ( at .gt. 0. ) THEN
                            at2 = at
			    IF  ( at .eq. 180. )  THEN
				drct = ' '
			      ELSE
				drct = 'E'
			    END IF
                          ELSE
                            at2 = at
                            drct = ' ' 
 			END IF
		        CALL GR_LABL ( at2, 0, 0, string, len, ier )
                        len = len + 1
			string ( len:len ) = drct
		        jshft = -len
		      ELSE
		        CALL GR_LABL ( at, 0, 0, string, len, ier )
		        jshft = -len
		    END IF
C
C*		    Plot the label.
C
		    CALL GTRANS ( 'M', 'V', 1,  side, at, x, y, ier) 
		    IF  ( ( x .gt. vlp .and. x .lt. vrp ) .and.
     +			  ( y .gt. vbp .and. y .lt. vtp ) )
     +			CALL GTEXT ( 'V', x, y, string (1:len), 0.,
     +				     jshft, -1, ier )
		END IF
	    END DO
	END IF
C*
	RETURN
	END
