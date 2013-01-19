	SUBROUTINE AF_HHFM  ( chhf, rhf, iret )
C************************************************************************
C* AF_HHFM								*
C*									*
C* This subroutine decodes a 3-digit string containing a height value	*
C* in units of hundreds-of-feet into a real height value in units of	*
C* feet.  On output, RHF = RMISSD if CHHF was not successfully	        *
C* decoded.								*
C*									*
C* AF_HHFM  ( CHHF, RHF, IRET )						*
C*									*
C* Input parameters:							*
C*	CHHF		CHAR*3		Encoded	height in ft*100	*
C*									*
C* Output parameters:							*
C*	RHF		REAL		Decoded height in feet    	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP	 7/99	Changed output from meters to feet      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	chhf
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
	rhf = RMISSD
C
C*	Decode the string.
C
	lchhf = LEN ( chhf )
	IF  ( lchhf .eq. 3 )  THEN
	    CALL ST_INTG  ( chhf (1:3), ihhf, ier )
	    IF  ( ier .eq. 0 )  THEN
		rhf = FLOAT ( ihhf ) * 100.0
	    END IF
	END IF
C*
	RETURN
	END
