	SUBROUTINE GH_KGFL ( alat, alon, ddate, nknt, tzone, dellon, 
     +			     xkey, ykey, iret )
C************************************************************************
C* GH_KGFL								*
C*									*
C* This subroutine creates the hour and day of the week string for the  *
C* current and forecast track positions and plots the labels.		*
C*									*
C* GH_KGFL ( ALAT, ALON, DDATE, NKNT, TZONE, DELLON, XKEY, YKEY, IRET ) *
C*									*
C* Input parameters:							*
C*	ALAT (NKNT)	REAL		Current/track latitudes		*
C*	ALON (NKNT)	REAL		Current/track longitudes	*
C*	DDATE (*)	CHAR*		Current and forecast times      *
C*      NKNT		INTEGER		Number of position points	*
C*      TZONE		CHAR*		Storm's time zone location	*
C*	DELLON		REAL		Longitudinal extent of map      *
C*	XKEY (*)	REAL		Norm. x coords of label box     *
C*	YKEY (*)	REAL		Norm. y coords of label box     *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01   					*
C* A. Hardy/GSC		 6/01	Cleaned up fcst label placement         *
C* D. Kidwell/NCEP	 6/01	Fixed for month changeover, line width  *
C* A. Hardy/GSC		 6/01	Added GH_SAVE, GH_REST			*
C* D. Kidwell/NCEP	 4/02	Rewrote for label placement; cleaned up *
C* D. Kidwell/NCEP	 3/03	Increased allowable number of points    *
C* D. Kidwell/NCEP	12/05	Fixed for year changeover               *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	ddate (*) , tzone
        REAL		alat (*), alon (*), xkey (*), ykey (*)
C*
	PARAMETER	( NUMHR = 8 )
	PARAMETER	( NUMPRV = 5 )
        CHARACTER       pos1*20, fmonth*9, fmon*9, fday*9, ctime*2,
     +                  zone2*3, ampm*2, tdate*20, mm*2, yymm*4,
     +			yyy*2
	INTEGER		jtarr(5)
        REAL		xx (NUMHR), yy (NUMHR), xlab (5,NUMPRV), 
     +			ylab (5,NUMPRV), xlin (2,NUMPRV), 
     +			ylin (2,NUMPRV)
        LOGICAL         plot (NUMHR), first, done, flip
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
      	iret = 0
C
C*	Query attributes.
C
        CALL GH_SAVE ( ier )
C
	CALL GSLINE ( 1, 0, 1, 0, ier )
        CALL GSTEXT ( 22, 2, 1.15, 1, 111, 1, 2, ier )
C
C*	Increase label offsets (given in nautical miles) for large
C*	geographic extents.
C
	IF ( dellon .le. 15. ) THEN
	    fact = .6
	  ELSE IF ( dellon .lt. 25. ) THEN
	    fact = .8
	  ELSE IF ( dellon .le. 40. ) THEN
	    fact = 1.
	  ELSE IF ( dellon .le. 50. ) THEN
	    fact = 1.4
	  ELSE
	    fact = 1.8
	END IF
	off1   = 120. * fact
	offint = 105. * fact
	offknt = 75.  * fact    
C
C*	Convert the latitudes and longitudes to normal coordinates.
C
        CALL GTRANS ( 'M', 'N', nknt, alat, alon, xx, yy, ier )
C
        DO ii = 1, nknt
C
C*	    Determine which points are to be labelled.
C
            plot ( ii )  = .true.
            IF ( ii .eq. 1 ) THEN
C
C*		Always label first point.
C
              ELSE IF ( ( ( ii .eq. 2 ) .and. ( nknt .gt. 3 ) ) .or.
     +			( ( ii .eq. 4 ) .and. ( nknt .gt. 4 ) ) ) THEN
C
C*		Do not routinely label second or fourth point.
C
                plot ( ii ) = .false. 
              ELSE IF ( ( ( alat (ii) .eq. alat (1) ) .and.
     +                    ( alon (ii) .eq. alon (1) ) ) .or.
     +                  ( ( alat (ii) .eq. alat (nknt) ) .and.
     +                    ( alon (ii) .eq. alon (nknt) ) .and.
     +                    ( ii .lt. nknt ) ) ) THEN
C
C*		Do not label point if same as first or last point.
C
                plot ( ii ) = .false. 
              ELSE 
C
C*		Check for point too close to first or last point.
C
                ydist = ABS ( yy (ii) - yy (1) )
                xdist = ABS ( xx (ii) - xx (1) )
                IF ( ( xdist .lt. 0.02 ) .and. (ydist .lt. 0.02 ) ) THEN
		    IF ( ii .lt. nknt )  plot ( ii ) = .false. 
                  ELSE IF ( ii .lt. nknt ) THEN
                    ydist = ABS ( yy (ii) - yy (nknt) )
                    xdist = ABS ( xx (ii) - xx (nknt) )
                    IF ( ( xdist .lt. 0.02 ) .and. (ydist .lt. 0.02 ) ) 
     +                     plot ( ii ) = .false.
                END IF
C
C*              Check for interior points too close together.
C
                IF ( ( ii .ge. 5 ) .and. ( ii .lt. nknt ) ) THEN
		    DO jj = 3, ii - 1
			IF ( plot ( jj ) ) THEN
                    	    ydist = ABS ( yy (ii) - yy (jj) )
                    	    xdist = ABS ( xx (ii) - xx (jj) )
                    	    IF ( ( xdist .lt. 0.02 ) .and.
     +			         ( ydist .lt. 0.02 ) )
     +                           plot ( ii ) = .false. 
			END IF
		    END DO
                END IF     
            END IF	
	END DO
C
C*	See if point 2 or 4 should be labelled.
C
	nlabel = 0
	DO ii = 1, nknt
	    IF ( plot ( ii ) ) nlabel = nlabel + 1
	END DO
	IF ( ( nlabel .eq. 2 ) .and. ( nknt .gt. 3 ) ) THEN
	    DO ii = 2, 4, 2
		ydist = ABS ( yy (ii) - yy (1) ) 
		xdist = ABS ( xx (ii) - xx (1) )
		IF ( ( xdist .gt. .05 ) .or. ( ydist .gt. .05 ) ) THEN
		    ydist = ABS ( yy (ii) - yy (nknt) ) 
		    xdist = ABS ( xx (ii) - xx (nknt) )
		    IF ( ( xdist .gt. .05 ) .or. ( ydist .gt. .05 ) )
     +			   plot ( ii ) = .true.
		END IF
	    END DO
	    ydist = ABS ( yy (2) - yy (4) ) 
	    xdist = ABS ( xx (2) - xx (4) )
	    IF ( ( xdist .lt. .02 ) .and. ( ydist .lt. .02 ) ) THEN
		IF ( nknt .gt. 4 ) THEN
      		    plot ( 4 ) = .false.
		  ELSE
		    plot ( 2 ) = .false.
		END IF
	    END IF
	END IF
C
C*	Loop over all track points.
C
	nlabel = 0
        DO ii = 1, nknt
            IF ( plot ( ii ) ) THEN 
		flip = .true.
C
C*              Get the location for the label.
C
                IF ( ( ii .gt. 1 ) .and. ( ii .lt. nknt ) ) THEN
C
C*		    This is an intermediate point.
C
		    done = .false.
		    ip   = ii - 1
		    dir1 = RMISSD
		    DO WHILE ( .not. done )
			IF ( ip .lt. 1 ) THEN
			    done = .true.
			  ELSE
			    IF ( ( alat ( ii ) .ne. alat ( ip ) ) .or.
     +				 ( alon ( ii ) .ne. alon ( ip ) ) ) THEN
				CALL CLO_DIRECT ( alat (ip), alon (ip),
     +				       		  alat (ii), alon (ii),
     +						  dir1, ier )
				done = .true.
			      ELSE
				ip = ip - 1
			    END IF
			END IF
		    END DO
C
		    done = .false.
		    ip   = ii + 1
		    dir2 = RMISSD
		    DO WHILE ( .not. done )
			IF ( ip .gt. nknt ) THEN
			    done = .true.
			  ELSE
			    IF ( ( alat (ii) .ne. alat (ip) ) .or.
     +				 ( alon (ii) .ne. alon (ip) ) ) THEN
				CALL CLO_DIRECT ( alat (ip), alon (ip),
     +					 	  alat (ii), alon (ii),
     +					 	  dir2, ier )
				done = .true.
			      ELSE
			        ip = ip + 1
			    END IF
			END IF
		    END DO
C
		    IF ( ERMISS ( dir1 ) .and. ERMISS ( dir2 ) ) THEN
			plot ( ii ) = .false.
		      ELSE IF ( ERMISS ( dir1 ) ) THEN
			side = -side
			dir = dir2 + side * 90.
		      ELSE IF ( ERMISS ( dir2 ) ) THEN
			side = -side
			dir  = dir1 + 180. + side * 90.
		      ELSE
			dir = ( dir1 + dir2 ) * .5
			angle = ABS ( dir1 - dir2 )
			IF ( angle .lt. 180. )  dir = dir + 180.
			IF ( ( angle .lt. 135. ) .or.
     +			     ( angle .gt. 225. ) ) flip = .false. 
		    END IF
C
		    IF ( plot ( ii ) ) THEN
			IF ( dir .ge. 360. ) dir = dir - 360.
			IF ( dir .lt. 0. )   dir = dir + 360.
                        offset = PR_HGNM ( offint )
		    END IF
                  ELSE IF ( ii .eq. 1 ) THEN
C
C*		    This is the first point.
C
		    done = .false.
		    ip   = 2
		    DO WHILE ( .not. done )
			IF ( ip .gt. nknt ) THEN
			    done = .true.
			    dir  = 270.
			  ELSE
			    IF ( ( alat ( 1 ) .ne. alat ( ip ) ) .or.
     +				 ( alon ( 1 ) .ne. alon ( ip ) ) ) THEN
				CALL CLO_DIRECT ( alat (ip), alon (ip),
     +					 alat (1), alon (1), dir, ier )
				done = .true.
			      ELSE
				ip = ip + 1
			    END IF
			END IF
		    END DO
                    offset = PR_HGNM ( off1 )
		    side   = -1.
 		    dir    = dir + side * 90.
                  ELSE
C
C*		    This is the last point.
C
		    done = .false.
		    ip   = nknt - 1
		    DO WHILE ( .not. done )
			IF ( ip .lt. 1 ) THEN
			    done = .true.
			    dir  = 270.
			  ELSE
			    IF ( ( alat (nknt) .ne. alat (ip) ) .or.
     +				 ( alon (nknt) .ne. alon (ip) ) ) THEN
				CALL CLO_DIRECT ( alat (nknt), 
     +					 	alon (nknt), alat (ip), 
     +					 	alon (ip), dir, ier )
				done = .true.
			      ELSE
				ip = ip - 1
			    END IF
			END IF
		    END DO
                    offset = PR_HGNM ( offknt )
		    side = -side
 		    dir    = dir + side * 90.
                END IF
C
		IF ( plot ( ii ) ) THEN
C
C*	            Create the day/time label string.
C
		    IF ( ii .eq. 1 ) THEN
		        yymm = ddate ( 1 ) ( 1:4 )
		        tdate = ddate ( 1 )
		        CALL ST_INTG ( ddate ( 1 ) ( 5:6 ), iday, ier )
		        first = .true.
		      ELSE
	    	        CALL ST_INTG ( ddate ( ii ) ( 1:2 ), nday, ier )
	    	        IF ( ( nday .lt. iday ) .and. first ) THEN
			    first = .false.
			    CALL ST_INTG ( ddate (1) (3:4), month, ier )
			    month = month + 1
			    IF ( month .gt. 12 ) THEN
				month = 1
				CALL ST_INTG ( ddate ( 1 ) ( 1:2 ), 
     +					       iyear, ier ) 
				iyear = iyear + 1
				CALL ST_INCH ( iyear, yyy, ier ) 
				IF ( iyear .lt. 10 ) 
     +				     yyy = '0' // yyy ( 1:1 )
				yymm ( 1:2 ) = yyy
			    END IF
			    CALL ST_INCH ( month, mm, ier )
			    IF ( month .lt. 10 ) mm = '0' // mm ( 1:1 )
			    yymm ( 3:4 ) = mm
	    	        END IF
                        tdate = yymm // ddate ( ii )
		    END IF
 	            CALL GH_TIME ( tdate, tzone, jtarr, hours, zone2,
     +                             fmonth, fmon, fday, ampm, ier )
	            CALL ST_INCH ( jtarr (4), ctime, ier )
	            CALL ST_LSTR ( ctime, lent, ier )
	            CALL ST_LSTR ( ampm, lena, ier )
                    pos1 = ctime (:lent) // ' ' // ampm (:lena) // 
     +                     ' ' // fday (1:3)
C
C*		    Check for overlap with previous labels, key, track.
C*		    Plot the label and label-connecting line if a good
C*		    location is found.
C
		    CALL GH_KGOV ( alat, alon, ii, offset, dir, nknt,
     +				   flip, xx, yy, xkey, ykey, pos1,
     +				   nlabel, xlab, ylab, xlin, ylin, ier )
		END IF
            END IF
        END DO
C
C*	Restore attributes.
C
	CALL GH_REST ( ier )
C*
	RETURN
	END
