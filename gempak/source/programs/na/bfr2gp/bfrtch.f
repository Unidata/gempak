	SUBROUTINE BFRTCH ( trange, time, ok, iret )
C************************************************************************
C* BFRTCH								*
C*									*
C* This subroutine checks for a station within time bounds.		*
C*									*
C* BFRTCH  ( TRANGE, TIME, OK, IRET )					*
C*									*
C* Input parameters:							*
C*	TRANGE (2)	CHAR*		Time range			*
C*	TIME		CHAR*		Station time			*
C*									*
C* Output parameters:							*
C*	OK		LOGICAL		Flag (true if station is in)	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* K. Brill/EMC		 2/97						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	trange(2), time
	LOGICAL		ok
C*
C-----------------------------------------------------------------------
	iret = 0
	ok = .false.
C*
	IF ( time .ge. trange (1) .and. time .le. trange (2) ) THEN
	    ok = .true.
	    RETURN
	END IF
	IF ( time .le. trange (1) .and. time .ge. trange (2) ) THEN
	    ok = .true.
	    RETURN
	END IF
	RETURN
	END
