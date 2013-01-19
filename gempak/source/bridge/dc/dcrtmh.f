	SUBROUTINE DC_RTMH  ( irundt, irpthr, irptmn, nhrb, nhra,
     +			      irptdt, iret )
C************************************************************************
C* DC_RTMH								*
C*									*
C* This subroutine combines a run date-time, report hour, and report	*
C* minute into a report date-time.  The report hour must be within	*
C* NHRB hours before the run hour or NHRA hours after the run hour.	*
C* The maximum allowable value for NHRB is 13, while the maximum	*
C* allowable value for NHRA is 7.					*
C*									*
C* DC_RTMH  ( IRUNDT, IRPTHR, IRPTMN, NHRB, NHRA, IRPTDT, IRET )	*
C*									*
C* Input parameters:							*
C*	IRUNDT (5)	INTEGER		Run date-time			*
C*					= (YYYY, MM, DD, HH, MM ) 	*
C*	IRPTHR		INTEGER		Report hour 			*
C*	IRPTMN		INTEGER		Report minute 			*
C*	NHRB 		INTEGER		Maximum number of hours earlier	*
C*                                      than run hour for report hour	*
C*	NHRA 		INTEGER		Maximum number of hours later	*
C*                                      than run hour for report hour	*
C*									*
C* Output parameters:							*
C*	IRPTDT (5)	INTEGER		Report date-time		*
C*					= (YYYY, MM, DD, HH, MM ) 	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = could not compute IRPTDT	*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	UT_RTMH -> AF_RTMH, style changes	*
C* R. Hollern/NCEP      10/00   Renamed AF_RTMH to DC_RTMH              *
C************************************************************************
	INTEGER		irundt (*), irptdt (*)
C*
	PARAMETER	( MXHRB = 13, MXHRA = 7 )
	INTEGER		iwrkdt (5)
C-----------------------------------------------------------------------
	iret = -1
C
C*	Store default values for the report date-time.
C
	irptdt (1) = irundt (1)
	irptdt (2) = irundt (2)
	irptdt (3) = irundt (3)
	irptdt (4) = irpthr
	irptdt (5) = irptmn
C
	IF  ( irundt (4) .eq. irpthr )  THEN
C
C*	    The report hour and run hour are the same, so no further
C*	    action is required.
C
	    iret = 0
	    RETURN
	END IF
C 
C*	Subtract as many as MXHRB hours from the run date-time.
C*	For each resulting date-time that is computed, check if the
C*	hour matches the report hour and, if so, then use the
C*	resulting year, month, and day as the report year, month,
C*	and day.
C
	DO i = 1, 5
	    iwrkdt (i) = irundt (i)
	END DO
C
	IF  ( nhrb .lt. MXHRB )  THEN
	    nb = nhrb
	ELSE
	    nb = MXHRB 
	END IF
C
        DO ii = 1, nb 
	    CALL TI_SUBM  ( iwrkdt, 60, iwrkdt, iersbm )
	    IF  ( iersbm .ne. 0 )  THEN
		RETURN
	    ELSE IF  ( iwrkdt (4) .eq. irpthr )  THEN
		irptdt (1) = iwrkdt (1)
		irptdt (2) = iwrkdt (2)
		irptdt (3) = iwrkdt (3)
		iret = 0
		RETURN
	    END IF
	END DO
C 
C*	Add as many as MXHRA hours to the run date-time.
C*	For each resulting date-time that is computed, check if the
C*	hour matches the report hour and, if so, then use the
C*	resulting year, month, and day as the report year, month,
C*	and day.
C
	DO i = 1, 5
	    iwrkdt (i) = irundt (i)
	END DO
C
	IF  ( nhra .lt. MXHRA )  THEN
	    na = nhra
	ELSE
	    na = MXHRA 
	END IF
C
        DO ii = 1, na 
	    CALL TI_ADDM  ( iwrkdt, 60, iwrkdt, ieradm )
	    IF  ( ieradm .ne. 0 )  THEN
		RETURN
	    ELSE IF  ( iwrkdt (4) .eq. irpthr )  THEN
		irptdt (1) = iwrkdt (1)
		irptdt (2) = iwrkdt (2)
		irptdt (3) = iwrkdt (3)
		iret = 0
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
