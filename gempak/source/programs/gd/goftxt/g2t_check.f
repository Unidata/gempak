	SUBROUTINE G2T_CHECK ( ktype, kwave, kwind, wok, sok, iret )
C************************************************************************
C* G2T_CHECK								*
C*									*
C* This subroutine checks if the range of wind/wave heights lies within	*
C* guideline.								*
C*									*
C* G2T_CHECK ( KTYPE, KWAVE, KWIND, WOK, SOK, IRET )			*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Grid parameter type		*
C*					 0 = wave and wind		*
C*					 1 = wave			*
C*					 2 = wind			*
C*	KWAVE (2)	INTEGER		Wave min/max			*
C*	KWIND (2)	INTEGER		Wind min/max			*
C*									*
C* Output Parameters:							*
C*	WOK		LOGICAL		Wave range flag			*
C*	SOK		LOGICAL		Wind range flag			*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		11/06						*
C* T. Lee/SAIC		11/06	Handled single values			*
C* T. Lee/SAIC		07/07	Added NCNTER check			*
C* T. Lee/SAIC		11/07	Table-driven range values		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	INTEGER		kwave (2), kwind (2)
	LOGICAL		wok, sok, contin
C-----------------------------------------------------------------------
	iret = 0
C
	wok = .true.
	sok = .true.
	kdwave = ABS ( kwave ( 2 ) - kwave ( 1 ) )
	kdwind = ABS ( kwind ( 2 ) - kwind ( 1 ) )
C
C*	Wave heights.
C
	IF  ( ktype .le. 1 )  THEN
	    kmax  = AMAX0 ( kwave ( 1 ), kwave ( 2 ) )
	    IF ( ncnter .eq.  1 )  THEN
		ki = 1
		contin = .true.
		DO WHILE ( contin )
		    IF ( kmax .ge. mnwgap ( ki, 1 ) .and.
     +			 kmax .lt. mnwgap ( ki, 2 ) )  THEN
			IF ( kdwave .gt. mnwgap (ki, 3) )  wok = .false.
			ki = ki + 1
		      ELSE
			ki = ki + 1
		    END IF
		    IF ( ki .gt. llwgap ) contin = .false.
		END DO
	      ELSE IF ( ncnter .eq. 2 )  THEN
		kmin = AMIN0 ( kwave ( 1 ), kwave ( 2 ) )
		kdif = kmax - kmin
		IF  ( kdif .gt. NINT ( kmin / 2. ) ) wok = .false.
	    END IF
	END IF
C
C*	Wind speed.
C
	IF ( ktype .eq. 0 .or. ktype .eq. 2 )  THEN
	    kmax  = AMAX0 ( kwind ( 1 ), kwind ( 2 ) )
	    kj = 1
	    contin = .true.
	    DO WHILE ( contin )
		IF ( kmax .gt. mnsgap ( kj, 1 ) .and.
     +		     kmax .le. mnsgap ( kj, 2 ) )  THEN
		    IF ( kdwind .gt. mnsgap ( kj, 3 ) )  sok = .false.
		    kj = kj + 1
		  ELSE
		    kj = kj + 1
		END IF
		IF ( kj .gt. llsgap ) contin = .false.
	    END DO
	END IF
C*		
	RETURN
	END
