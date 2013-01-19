	SUBROUTINE POLY_RMFLG ( nk, xclip, yclip, rmflg, iret )
C************************************************************************
C* POLY_RMFLG  								*
C*									*
C* This subroutine sets remove flags for clipping.  The remove flag	*
C* is set to .FALSE. if the clipping point is on the map bounds. That	*
C* is, the point won't be removed after point reductions.		*
C*									*
C* POLY_RMFLG ( NK, XCLIP, YCLIP, RMFLG, IRET )				*
C*									*
C* Input parameters:							*
C*	NK		INTEGER		Number of clipped points	*
C*	XCLIP (NK)	REAL		Latitudes of clipped points	*
C*	YCLIP (NK)	REAL		Longitudes of clipped points	*
C*									*
C* Output parameters:							*
C*	RMFLG (NK)	LOGICAL		Remove flag			*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		05/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	PARAMETER	( EPS = .001 )
	REAL		xclip(*), yclip(*)
	LOGICAL		done, betwn, on, rmflg(*)
C-----------------------------------------------------------------------
	iret = 0
C
C*	Set remove flag for the clipped points.
C
	DO kk = 1, nk
	    ii = 1
	    done = .false.
	    DO WHILE ( .not. done ) 
		betwn = .false.
		on = .false.
		xc = xclip ( kk )
		yc = yclip ( kk )
		xp1 = zzlat ( ii )
		yp1 = zzlon ( ii )
		xp2 = zzlat ( ii + 1 )
		yp2 = zzlon ( ii + 1 )
		A = yc - yp1
		B = xc - xp1
		ang1 = ATAN2 ( A, B )
		C = yp2 - yc
		D = xp2 - xc
		ang2 = ATAN2 ( C, D )
C
		betwn = ( ( ( xc - xp1 ) * ( xc - xp2 ) ) .lt. 0. )
     +			.and. ( ABS ( ang1 - ang2 ) .le. EPS )
C
     		on = ( ABS ( A ) .le. EPS ) .and. 
     +		     ( ABS ( B ) .le. EPS )
C
		IF ( betwn .or. on ) THEN
		    rmflg ( kk ) = .FALSE.
		    done = .true.
		  ELSE
		    ii =  ii + 1 
		    IF ( ii .gt. ( npzon - 1 ) ) done = .true.
		END IF
	    END DO
	END DO
C
	RETURN
C*
	END
