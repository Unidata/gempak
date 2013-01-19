	SUBROUTINE GH_KGAR ( xmnlat, xmnlon, xmxlat, xmxlon, 
     +                       rmnlat, rmnlon, rmxlat, rmxlon, iret )
C************************************************************************
C* GH_KGAR								*
C*									*
C* This subroutine determines whether the GAREA for the tropical cyclone*
C* watch/warn graphic needs to be enlarged to contain a familiar US     *
C* Atlantic coastal area.		                                *
C*									*
C* GH_KGAR ( XMNLAT, XMNLON, XMXLAT, XMXLON, RMNLAT, RMNLON, RMXLAT,    *
C*           RMXLON, IRET )						*
C*									*
C* Input Parameters:							*
C*	XMNLAT 		REAL		Minumum latitude		*
C*	XMNLON 		REAL		Mininum longitude		*
C*	XMXLAT 		REAL		Maximum latitude		*
C*	XMXLON 		REAL		Maximum longitude		*
C*									*
C* Output Parameters:							*
C*	RMNLAT 		REAL		New Minumum latitude		*
C*	RMNLON 		REAL		New Mininum longitude		*
C*	RMXLAT 		REAL		New Maximum latitude		*
C*	RMXLON 		REAL		New Maximum longitude		*
C*	IRET		INTEGER		Return code			*
C*					    0 = normal return           *
C*					   -1 = can not find US coast	*
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01						*
C* A. Hardy/GSC		 6/01	Reverse check for high lat. storms	*
C* D. Kidwell/NCEP	 4/04	Updated prolog                          *
C* X. Guo/CWS		05/10   Changed delta value to reduce the       *
C*                              increment of the new lat/lon            *  
C************************************************************************
	REAL  		xmnlat, xmnlon, xmxlat, xmxlon, 
     +                  rmnlat, rmnlon, rmxlat, rmxlon
C*
	REAL		xpts(12), ypts(12)
	LOGICAL         done, finish
C*
	DATA	ypts / 25.90, 29.52, 29.98, 30.22, 27.97, 24.55,
     +                 30.50, 32.90, 35.22, 39.13, 40.80, 42.37 /
C
	DATA	xpts / -97.43, -95.24, -90.25, -85.68, -82.53, -81.75, 
     +                 -81.70, -80.03, -75.62, -75.47, -73.10, -71.03 /
C------------------------------------------------------------------------
	iret = 0
        ipass = 1
        finish = .false.
C
        DO WHILE ( .not. finish ) 
            nit = 1
            delta = 5.
            done = .false.
C
C*	    Set output lat/lons.
C
	    rmnlat = xmnlat
	    rmnlon = xmnlon
	    rmxlat = xmxlat
	    rmxlon = xmxlon
C
C*	    Check to see if any of the 12 cities are in the GAREA.
C
	    DO WHILE ( .not. done )
                ii = 1
                DO WHILE ( ii .le. 12)
C
C*	            Set counter for forward and backward searches.
C
	            IF ( ipass .eq. 1 ) THEN
                        jj = ii
	              ELSE IF ( ipass .eq. 2 ) THEN
                        jj = 13 - ii
                    END IF
C
	            IF ( ( xpts(jj) .ge. rmnlon ) .and. 
     +                   ( xpts(jj) .le. rmxlon ) .and. 
     +                   ( ypts(jj) .ge. rmnlat ) .and. 
     +                   ( ypts(jj) .le. rmxlat ) ) THEN
		        done = .true.
                        finish = .true.
                    END IF
                    ii = ii + 1
                END DO
C
                IF ( .not. done ) THEN
                    IF ( ipass .eq. 1 ) THEN
                        rmxlat = rmxlat + delta/2
                      ELSE IF ( ipass .eq. 2 ) THEN
                        rmnlat = rmnlat - delta/2
                    END IF
                    rmnlon = rmnlon - delta
                END IF
C
                IF ( nit .gt. 6 ) THEN
                    IF ( .not. done ) THEN
                        done = .true.
                        IF ( ipass .eq. 2 ) THEN
                            iret = -1
                            finish = .true.
                        END IF
                    END IF
                  ELSE
                    nit = nit + 1
                    delta = delta + 1.
                END IF
            END DO
            ipass = ipass + 1
        END DO
C*
	RETURN
	END
