	FUNCTION PR_CSYH ( ctyh )
C************************************************************************
C* PR_CSYH								*
C*									*
C* This function will compute the cloud type symbol number from the 	*
C* synoptic code cloud type number.					*
C*									*
C* REAL PR_CSYH ( CTYH )						*
C*									*
C* Input parameters:							*
C*	CTYH		REAL		Synoptic code for cloud type	*
C*									*
C* Output parameters:							*
C*	PR_CSYH		REAL		Cloud symbol number		*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 9/91						*
C* K. Brill/NMC		10/91	New names; return zero for missing	*
C************************************************************************
	IF  ( ctyh .ge. 1 .and. ctyh .le. 9 )  THEN
	    PR_CSYH = ctyh + 20.
	  ELSE
	    PR_CSYH = 0.0
	ENDIF
C*
	RETURN
	END
