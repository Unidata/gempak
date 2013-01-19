	SUBROUTINE IS_CNAR ( report, lenr, mxp, origin, npt, rlat, rlon,
     +			     irad, iptr, iret )
C************************************************************************
C* IS_CNAR 								*
C*									*
C* This subroutine gets the boundary points of the area covered by a	*
C* Canadian SIGMET phenomenon.                            		*
C*                                                                      *
C* IS_CNAR ( REPORT, LENR, MXP, ORIGIN, NPT, RLAT, RLON, IRAD, IPTR,	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*	MXP		INTEGER		Maximum number of points        *
C*	ORIGIN		CHAR*		Originating station ID		*
C*									*
C* Output parameters:							*
C*	NPT 		INTEGER		Number of boundary points       *
C*      RLAT(*)		REAL		Latitudes of boundary points    *
C*	RLON(*)		REAL		Longitudes of boundary points   *
C*	IRAD		INTEGER		Radius if area is a circle      *
C*	IPTR		INTEGER		Pointer to location after area  *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad area definition       *
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	10/03	Created from IS_AREA.			*
C* F. J. Yen/NCEP	12/03	Increased search limit from 260 to 460.	*
C* F. J. Yen/NCEP	12/03	Allowed for no "/" in area def.		*
C* F. J. Yen/NCEP	 3/04	Shorten string report in call to ST_RPSL*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, origin
	REAL		rlat (*), rlon (*)
C*
	PARAMETER       ( MAXPTS = 20 )
	CHARACTER	carr (20)*12, work*20, card*2
	CHARACTER	trep*1250
	REAL		tlat (MAXPTS), tlon (MAXPTS)
	LOGICAL		done, latlon, aside
C------------------------------------------------------------------------
	iret = 0
	iptr = 0
	npt  = 0
	irad = IMISSD
	DO i = 1, mxp
	    rlat ( i ) = RMISSD
	    rlon ( i ) = RMISSD
	END DO 
C
C*	Look for area definition beginning with 'WTN' or "WITHIN'
C
	len = MIN ( 460, lenr ) 
	iloc = INDEX ( report ( :len ), ' WTN ' )
	IF ( iloc .eq. 0 ) THEN
	    iloc = INDEX ( report ( :len ), ' WITHIN ' )
	    IF ( iloc .eq. 0 ) THEN
		iret = -4
		RETURN
	      ELSE
	        iloc = iloc + 7
	    END IF
	  ELSE
	    iloc = iloc + 4
	END IF
C
C*	Determine if this is a circle, a bounded area, or either side of
C*	line.
C
C* 	First, check for distance (in nm) from a line or from center.
C
	lenn = MIN (iloc+10, lenr )
	inm   = INDEX ( report ( iloc:lenn ), 'NM ' )
	aside = .false.
	IF ( inm .eq. 0 ) THEN
C
C*	    Check for 'AREA BOUNDED'
C
	    lenn = MIN (iloc+18, lenr )
	    ibnd = INDEX ( report ( iloc:lenn ), ' AREA BOUNDED ' )
	    IF ( ibnd .eq. 0 ) THEN
		iret = -4
		RETURN
	    END IF
C
C*	    This is a bounded area
C
	    iside = -1
	    ilocsv = iloc + ibnd + 13
	  ELSE
C
C*	    Check if isolated or ESOL 
C
	    ibeg = -1
	    ien2 = iloc + inm -2
	    IF ( report ( ien2:ien2 ) .eq. CHSPAC ) ien2 = ien2 - 1
	    ibeg = MAX ( iloc, ien2 - 2 )
	    CALL ST_NUMB ( report ( ibeg:ien2 ), iside, ier )
	    IF ( ier .ne. 0 ) THEN
	        iret = -4
   		RETURN
            END IF
	    iln = iloc + inm + 2
	    IF ( ( INDEX ( report ( iln:len ), ' LN '	) .eq. 0 ) .and.
     +		 ( INDEX ( report ( iln:len ), 'LINE' ) .eq. 0 ) ) THEN
C    
C*		This is a circle. 
C
		irad  = iside
		iside = 0
    		ilocsv = iln -1
	      ELSE
C
C*		This is one or both sides of a line.
C
		nptt = 0
		ilocsv = iloc + inm + 4
		ilnmof = iloc + inm + 1
		inmof = INDEX ( report ( ilnmof - 6: ilnmof + 4 ),
     +						  'NM OF ' )
		inmeth = INDEX ( report ( ilnmof - 6: ilnmof + 8 ),
     +				                  'NM EITHER ' )
		IF ( inmof .eq. 0 .and. inmeth .eq. 0 ) THEN
C
C*		    Check to make sure a direction after ' NM '
C
		    ndxn = INDEX (report (ilnmof:ilnmof+2), ' N' )
		    ndxe = INDEX (report (ilnmof:ilnmof+2), ' E' )
		    ndxs = INDEX (report (ilnmof:ilnmof+2), ' S' )
		    ndxw = INDEX (report (ilnmof:ilnmof+2), ' W' )
		    ndir = ndxn + ndxe + ndxs + ndxw
		    IF ( ndir .ne. 0 ) THEN
			jil1 = ilnmof+ndir+1
			jil2 = ilnmof+ndir+8
			IF ( INDEX(report(ilnmof+ndir+1:ilnmof+ndir+8),
     +				' OF L' ) .ne. 0 ) THEN
			    card  = report ( ilnmof + 1:ilnmof + 2 )
			    aside = .true.
		          ELSE
		            iret = -4
		    	    RETURN
			END IF
		    END IF
		END IF 
	    END IF

	END IF
C
C*	Find the beginning and end of the point definition string.
C*	(Find beginning for latlon format)
C
	iend = INDEX ( report ( ilocsv:len+1 ), '. ' )
	IF ( iend .eq. 0 ) THEN
	    iret = -4
	    RETURN
	END IF
	iend = ilocsv + iend - 2
	IF ( iend .eq. 0 ) THEN
            iret = -4
            RETURN
        END IF
C
C*	Remove geographical references by invoking IS_CNRG
C*	and put into trep
C
	lent = iend - iloc + 1
	iloc = ilocsv
	CALL IS_CNRG ( report(iloc:iend), lent, 1250, trep, nlen, ier )
	IF ( ier .eq. 0 ) THEN
	    iend = lent
            latlon = .false.
            ii   = iloc
            done = .false.
            DO WHILE ( .not. done )
                CALL ST_ALNM ( report ( ii:ii ), ityp, ier )
                IF ( ityp .eq. 1 ) THEN
C
C*                  Check if numeric is for distance "NM" from a
C*                  geographical point
C
                    minii = MIN ( iend, ii+7 )
                    nmiles = INDEX ( report ( ii+1:minii+7 ), "NM" )
                    IF ( nmiles .eq. 0 ) THEN
                        done = .true.
                        latlon = .true.
                        ibeg = ii - 1
                      ELSE
                        ii = ii + 1
                        IF ( ii .gt. iend ) THEN
                            done = .true.
                        END IF
                    END IF
                  ELSE
                    ii = ii + 1
                    IF ( ii .gt. iend ) THEN
                        done = .true.
                    END IF
                END IF
            END DO
C
C*	Get the points.
C
	    iptr = iend + 3
	    IF ( ilocsv .le. iend ) THEN
	        iloc = ilocsv
C
C*	        Check for '-' and replace with space before breaking
C*	        into array of strings.
C
	        ipos = -1
	        DO WHILE ( ipos .ne. 0 )
                    CALL ST_RPSL ( trep(:nlen), '-', 1, ' ', 1, ipos,
     +                             trep, ier )
                END DO
	        lntrep = nlen
	        CALL ST_CLST ( trep ( :lntrep ), ' ', ' ', mxp, 
     +		               carr, npts, ier )
	      ELSE
	        npts = 0
	    END IF
	    ier = 0
	    ii = 1
	    IF ( latlon ) THEN
		DO WHILE ( ii .le. npts )
		    work = carr ( ii )
	    	    CALL ST_LSTR ( work, lenw, ier )
	    	    IF ( lenw .le. 5 ) THEN
	 		CALL ST_WORD ( work ( :lenw ), ityp, ier )
			IF ( ityp .eq. 1 ) THEN
		    	    ii   = ii + 1
		    	    work = work ( :lenw ) // carr ( ii )
			END IF
	    	    END IF
C
	    	    CALL IS_LTLN ( work, xlat, xlon, ier )
	    	    IF ( ier .eq. 0 ) THEN
			IF ( iside .le. 0 ) THEN
		    	    npt = npt + 1
		    	    rlat ( npt ) = xlat
		    	    rlon ( npt ) = xlon
			  ELSE 
		    	    IF ( nptt .lt. MAXPTS ) THEN
		        	nptt = nptt + 1
		        	tlat ( nptt ) = xlat
		        	tlon ( nptt ) = xlon
		    	    END IF
		        END IF 
	    	    END IF
	    	    ii = ii + 1
		END DO
	    END IF
	    IF ( .not. latlon .or. ier .ne. 0 ) THEN
	        IF ( latlon ) iloc = ilocsv
	        IF ( iside .le. 0 ) THEN
                    CALL IS_CBND ( report ( iloc:iend ), MAXPTS,
     +		            npt, rlat, rlon, iptr, ier )
		    IF ( ier .ne. 0 .or. npt .eq. 0 ) THEN
		        iret = -4
		        RETURN
		    END IF
	          ELSE
                    CALL IS_CBND ( report ( iloc:iend ), MAXPTS,
     +		            nptt, tlat, tlon, iptr, ier )
		    IF ( ier .ne. 0 .or. nptt .eq. 0 ) THEN
		        iret = -4
		        RETURN
		    END IF
	        END IF
    	    END IF 
C
	    IF ( iside .gt. 0 ) THEN
	        IF ( nptt .gt. 1 ) THEN
C
C*		    Get the points on either side or one side of a line.
C
		    IF ( .not. aside ) THEN
		        CALL IS_ESOL ( tlat, tlon, nptt, iside, rlat,
     +				       rlon, npt, iret )
		      ELSE
		        CALL IS_OSOL ( tlat, tlon, nptt, iside, card,
     +				       rlat, rlon, npt, iret )
		    END IF
	          ELSE
		    iret = -4
	        END IF
	      ELSE IF ( ( iside .lt. 0 ) .and. ( npt .eq. 0 ) ) THEN
C
C*	        Check for the configuration lat1 TO lat2 FROM lon1 TO lon2.
C
	        IF ( npts .lt. 7 ) THEN
		    done = .true.
		    iret = -4
	          ELSE
		    done = .false.
	            ii   = 2
	        END IF
	        DO WHILE ( .not. done )
		    IF ( ( carr (ii)   (:2) .eq. 'TO' ) .and.
     +		         ( carr (ii+2) (:4) .eq. 'FROM' ) .and.
     +		         ( carr (ii+4) (:2) .eq. 'TO' ) ) THEN
		        done = .true.
		        CALL IS_BDLN ( carr ( ii-1 ), carr ( ii+1 ),
     +				       carr ( ii+3 ), carr ( ii+5 ),
     +			               rlat, rlon, npt, iret )
	              ELSE
		        ii = ii + 1
		        IF ( ( ii + 5 ) .gt. npts ) THEN
		            done = .true.
		            iret = -4
		        END IF
		    END IF
	        END DO
	      ELSE IF ( ( iside .eq. 0 ) .and. ( npt .ne. 1 ) ) THEN
	        iret = -4
	    END IF
	  ELSE
C
C*	    Call IS_AREA when there are no slashes in the area definition
C
	    CALL IS_AREA ( report, lenr, mxp, origin, npt, rlat, rlon,
     +			   irad, iptr, iret )
	END IF
C*
	RETURN
	END
