        SUBROUTINE MA_SHIP ( ship, iret )
C************************************************************************
C* MA_SHIP                                                              *
C*                                                                      *
C* This subroutine checks the report to see if it is a ship report.     *
C* 								        *
C* MA_SHIP ( SHIP, IRET )                                               *
C*								        *
C* Output parameters:						        *
C*      SHIP           LOGICAL	  Flag for ship report type             *
C*      IRET           INTEGER    Return code                       	*
C*                                  0 = normal return			*
C**                                                                     *
C* Log:							                *
C* D. Kidwell/NCEP	 5/00	                                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE		'macmn.cmn'
C*
	LOGICAL		ship
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
        iret  = 0
C
C*	Assume a ship report to begin.
C
	ship = .true.
C 
	IF ( rivals ( irtost ) .eq. 0. ) THEN
C
C*	    If this is an automatic station not labelled as SHIP, treat
C*	    it as a non-ship report.
C
	    IF ( civals ( icstid ) .ne. 'SHIP' ) ship = .false.
	  ELSE
C
C*	    If this is a manned station with identifier 62nnn, 63nnn,
C*	    64nnn or 44145, or if the manned/automatic indicator is
C*	    missing and the identifier is all numeric, treat it as a 
C*	    non-ship report.
C
	    CALL ST_LSTR ( civals ( icstid ), lens, ier )
	    IF ( lens .eq. 5 ) THEN
		CALL ST_INTG ( civals ( icstid ) ( :5 ), intg, ier )
		IF ( ier .eq. 0 ) THEN
		    ii = intg / 1000
		    IF ( ( ( ii .ge. 62 ) .and. ( ii .le. 64 ) ) .or.
     +			 ( intg .eq. 44145 ) .or.
     +			 ( ERMISS ( rivals ( irtost ) ) ) ) 
     +			 ship = .false.
		END IF
	    END IF
	END IF
C*
	RETURN
	END
