	SUBROUTINE SS_WAIT  ( seconds, iret )
C************************************************************************
C* SS_WAIT								*
C*									*
C* This subroutine halts the execution of a program for up to 420	*
C* seconds (7 minutes).							*
C*									*
C* SS_WAIT  ( SECONDS, IRET )						*
C*									*
C* Input parameters:							*
C*	SECONDS		REAL		Number of seconds to wait	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  NORMAL = normal return	*
C**									*
C* Log: 								*
C* I. Graffman/RDS	10/87						*
C* M. desJardins/NMC	 3/92	UNIX version to call c subroutine	*
C* S. Jacobs/EAI	 8/93	Changed call to CSLEEP			*
C* T. Piper/SAIC	10/04	Changed input to float; csleep changed  *
C************************************************************************
	INCLUDE 'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Call C subroutine.  Note that calling the UNIX sleep routine
C*	directly can be dangerous.
C
	IF  ( ( seconds .gt. 0 ) .and. ( seconds .le. 420 ) )  THEN
	    CALL CSLEEP  ( seconds, ier )
	END IF
C
	RETURN
	END
