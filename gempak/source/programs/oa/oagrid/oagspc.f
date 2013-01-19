	SUBROUTINE OAGSPC  ( gltln, slat, slon, number, dscomp, dsunif, 
     +			     iret )
C************************************************************************
C* OAGSPC								*
C*									*
C* This subroutine computes the average minimum station spacing		*
C* and the uniform station spacing.					*
C*									*
C* OAGSPC  ( GLTLN, SLAT, SLON, NUMBER, DSCOMP, DSUNIF, IRET )		*
C*									*
C* Input parameters:							*
C*	GLTLN (4)	REAL		Grid bounds			*
C*	SLAT (NUMBER)	REAL		Station latitudes		*
C*	SLON (NUMBER)	REAL		Station longitudes		*
C*	NUMBER		INTEGER		Number of stations		*
C*									*
C* Output parameters:							*
C*	DSCOMP		REAL		Average min station spacing	*
C*	DSUNIF		REAL		Uniform station spacing		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = insufficient stations	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/85						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'oagcmn.cmn'                            
C*
	REAL		slat (*), slon (*), gltln (*)
C*
	PARAMETER	( EPS = 1.E-10, DIST = 8., BIG = 100000. )
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for insufficient stations.
C
	IF  ( number .lt. 4 )  THEN
	    iret = -9
	    CALL ER_WMSG  ( 'OAGRID', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Check each station and find the closest station.
C
	ns   = 0
	dsum = 0.
C*
	DO  i = 1, number
	    tlat  = slat (i)
	    tlon  = slon (i)
	    amind = big
	    tlatp = tlat + DIST
	    tlatm = tlat - DIST
	    tlonp = tlon + DIST
	    tlonm = tlon - DIST
	    DO  j = 1, number
		IF  ( i .ne. j )  THEN
		    ulat = slat (j)
		    ulon = slon (j)
	            IF ( ( ulat .gt. tlatm ) .and. ( ulat .lt. tlatp ) 
     +			 .and. ( ulon .gt. tlonm ) .and. 
     +                   ( ulon .lt. tlonp ) )  THEN
		        dc = COS  ( ( ( tlat + ulat ) / 2. ) * DTR )
		        d  = ( tlat - ulat ) ** 2 + 
     +                       ( dc * ( tlon - ulon ) ) ** 2
	  	        IF  ( ( d .lt. amind ) .and. 
     +                        ( d .gt. EPS ) )  amind = d
	            END IF
		END IF
	    END DO
C
C*	    Check that information is not missing.
C
	    IF  ( amind .lt. BIG )  THEN
	        dsum = dsum + SQRT ( amind )
	        ns   = ns + 1
	    END IF
	END DO
C
C*	Check that there are enough stations.
C
	IF  ( ns .lt. 2 )  THEN
	    iret = -9
	    CALL ER_WMSG  ( 'OAGRID', iret, ' ', ier )
	  ELSE
C
C*	    Compute the computed station spacing.
C
	    dscomp = dsum / ns
C
C*	    Compute the uniform station spacing.
C
	    dn = COS  ( ( gltln (3) + gltln (1) ) * DTR / 2. )
	    d1 = gltln (3) - gltln (1)
	    d2 = ( gltln (4) - gltln (2) ) * dn
	    a  = 1 - number
	    b  = d1 + d2
	    c  = d1 * d2
	    ds = SQRT ( b**2 - 4.*a*c )
	    dsunif = ( -b - ds ) / ( 2. * a )
	END IF
C*
	RETURN
	END

