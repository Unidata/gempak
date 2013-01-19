	SUBROUTINE GH_KGLC ( alatt, alont, offset, dir, blat, blon,
     +		             idir, just, ixoff, iyoff, iret )
C************************************************************************
C* GH_KGLC								*
C*									*
C* This subroutine gets the location, offsets and justification for a   *
C* day/time label string.                                               *
C*									*
C* GH_KGLC ( ALATT, ALONT, OFFSET, DIR, BLAT, BLON, IDIR, JUST, IXOFF,  *
C*	     IYOFF, IRET )                                              *
C*									*
C* Input parameters:							*
C*	ALATT		REAL		Track latitude			*
C*	ALONT		REAL		Track longitude			*
C*	OFFSET		REAL		Length of connecting line (m)   *
C*	DIR		REAL		Direction of connecting line    *
C*									*
C* Output parameters:							*
C*	BLAT		REAL		Label latitude                  *
C*	BLON		REAL		Label longitude			*
C*	IDIR		INTEGER		Adjusted connecting line direc. *
C*	JUST		INTEGER		Label justification             *
C*	IXOFF		INTEGER		Label x offset                  *
C*	IYOFF		INTEGER		Label y offset                  *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/02	                                        *
C* D. Kidwell/NCEP	 3/03	Added check for left edge               *
C* X. Guo/CWS	        05/10   Added check for logo                    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        REAL		dmnlat, dmnlon, dmxlat, dmxlon,dcurlat, dcurlon
C-----------------------------------------------------------------------
      	iret = 0
C
C*	Get the proposed label location.
C
        CALL CLO_DLTLN ( alatt, alont, offset, dir, blat, blon, ier )
C
C*      Make sure there are no horizontal or vertical label-connecting 
C*	lines, which could overplot other lines (lat/lon or political 
C*	bounds).
C
        IF ( ABS ( blon - alont ) .lt. .05 )  blon   = blon - .2
	IF ( ABS ( blat - alatt ) .lt. .05 )  blat   = blat + .2
C
C*      Check label lat/lon for overlap of logo
C
        CALL GQBND ( 'V', dmnlat, dmnlon, dmxlat, dmxlon, ier )
        CALL GTRANS ( 'M', 'V', 1, blat,  blon, 
     +                dcurlat, dcurlon, ier )
        IF (dcurlon .le. (dmnlon + 0.13) ) THEN
            IF ( dcurlat .le. 0.14) THEN
                blat = ABS ( blat - alatt ) + alatt
                dir = dir - 180.
            ELSE
                IF (dcurlat .ge. (dmxlat-0.13) ) THEN
                     blat = ABS ( blat - alatt ) + alatt
                     dir = dir - 180.
                END IF
            END IF
        END IF
C
	idir  = NINT ( dir )
	IF ( idir .ge. 360 ) THEN
	    idir = idir - 360
	  ELSE IF ( idir .lt. 0 ) THEN
	    idir = idir + 360
	END IF
	idel = MOD ( idir, 90 )
	IF ( idel .lt. 5 ) THEN
	    idir = idir + 5
	    IF ( idir .ge. 360 ) idir = idir - 360
	  ELSE IF ( idel .gt. 85 ) THEN
	    idir = idir - 5
	    IF ( idir .lt. 0 ) idir = idir + 360
	END IF
C
C*	Justification and offsets will be dependent on the
C*	direction of the label-connecting line.
C
        CALL GTRANS ( 'M', 'N', 1, blat, blon, bxx, byy, ier )
	IF ( idir .ge. 180 ) THEN
	    just  = 3
	    ixoff = -1
C
C*	    Check that label is far enough from left edge.
C
	    IF ( bxx .lt. .13 ) THEN
 		just  = 2
 		ixoff = -4
		IF ( bxx .lt. .10 ) THEN
		    ixoff = -2
		    IF ( bxx .lt. .07 ) ixoff = 0
		END IF
 	    END IF
	  ELSE
	    just  = 1
	    ixoff = 1
C
C*	    Check that label is far enough from right edge.
C
	    IF ( bxx .gt. .87 ) THEN
 		just  = 2
 		ixoff = 4
		IF ( bxx .gt. .90 ) THEN
		    ixoff = 2
		    IF ( bxx .gt. .93 ) ixoff = 0
		END IF
 	    END IF
	END IF
	IF ( ( idir .gt. 90 ) .and. ( idir .lt. 270 ) ) THEN
	    iyoff = -1
C
C*	    Check that label is far enough from bottom.
C
	    IF ( byy .lt. .03 ) THEN
		iyoff = 1
		IF ( byy .lt. .01 )  THEN
		    iyoff = 2
		    IF ( byy .lt. 0. ) iyoff = 3
		END IF
	    END IF
	  ELSE
	    iyoff = 1
	END IF
C*
	RETURN
	END
