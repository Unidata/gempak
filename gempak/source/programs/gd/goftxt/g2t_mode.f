	SUBROUTINE G2T_MODE ( nmode, mode, cmode, imode, iret )
C************************************************************************
C* G2T_MODE	 							*
C*									*
C* This subroutine returns most prevalent wind directions in descending	*
C* order.								*
C* 									*
C* G2T_MODE ( NMODE, MODE, CMODE, IRET )				* 
C*									*
C* Input parameters:							*
C*	NMODE		INTEGER		Number of wind directions	*
C*	MODE		INTEGER		8-point wind direction		* 
C*									*
C* Output parameters:							*
C*	CMODE		CHARACTER	Prevalent wind in compass dir.	*
C*	IMODE		INTEGER		Prevalent wind in 8-point dir.	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/06						*
C************************************************************************
	INTEGER		mode ( nmode ), imode (nmode)
	CHARACTER*2	cmode ( nmode ), ctemp
C-----------------------------------------------------------------------
	iret = 0
C
C*	Initialize output.
C
	cmode ( 1 ) = 'NE'
	cmode ( 2 ) = 'E'
	cmode ( 3 ) = 'SE'
	cmode ( 4 ) = 'S'
	cmode ( 5 ) = 'SW'
	cmode ( 6 ) = 'W'
	cmode ( 7 ) = 'NW'
	cmode ( 8 ) = 'N'
C
	DO ii = 1, 8
	    imode ( ii ) = ii
	END DO
C
C*	Sort the prevalent wind directions.
C
	DO kx =  1, nmode - 1
	    DO ky = kx + 1, nmode
		IF  ( mode ( ky ) .gt. mode ( kx ) ) THEN
		    itemp = mode ( ky )
		    mode ( ky ) = mode ( kx )
		    mode ( kx ) = itemp
C
		    ctemp = cmode ( ky )
		    cmode ( ky ) = cmode ( kx )
		    cmode ( kx ) = ctemp
C
		    jtemp = imode ( ky )
		    imode ( ky ) = imode ( kx )
		    imode ( kx ) = jtemp
		END IF
	    END DO
	END DO
C
	RETURN
C*
	END
