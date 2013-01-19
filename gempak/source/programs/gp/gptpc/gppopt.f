	SUBROUTINE GPPOPT ( strmid, iret )
C************************************************************************
C* GPPOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* GPPOPT ( STRMID, IRET )						*
C*									*
C* Input Parameters:							*
C*	STRMID		CHAR*		Storm identifier		*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01	Copied from GPMOPT			*
C* D. Kidwell/NCEP	 6/01	Renamed from GPTOPT to GPPOPT           *
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
5000	FORMAT ( ' GPTPC PARAMETERS:',//
     +           ' Strmid:              ', A )
C*
	RETURN
	END
