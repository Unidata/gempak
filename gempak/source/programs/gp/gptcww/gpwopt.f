	SUBROUTINE GPWOPT ( strmid, iret )
C************************************************************************
C* GPWOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* GPWOPT ( STRMID, IRET )						*
C*									*
C* Input parameters:							*
C*	STRMID		CHAR*		Storm identifier		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/01	Copied from GPPOPT                      *
C* m.gamazaychikov/SAIC	03/04	Copied from GPKOPT                      *
C************************************************************************
	CHARACTER*(*)	strmid
C*
	LOGICAL		respnd
C------------------------------------------------------------------------
C 
	iret = 0
	WRITE ( 6, 5000) strmid
C 
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd ) THEN
	    CALL TM_ACCP ( ier )
	    IF ( ier .eq. 2 ) iret = -1
	END IF
C 
5000	FORMAT ( ' GPTCWW PARAMETERS:',//
     +           ' Strmid:              ', A )
C*
	RETURN
	END
