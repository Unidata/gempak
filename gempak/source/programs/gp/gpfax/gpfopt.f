	SUBROUTINE GPFOPT ( device, faxfil, iret )
C************************************************************************
C* GPFOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* GPFOPT ( DEVICE, FAXFIL, IRET )					*
C*									*
C* Input Parameters:							*
C*	DEVICE		CHAR*		Device				*
C*	FAXFIL		CHAR*		Product file			*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = error			*
C**									*
C* Log:									*
C* R. Tian/SAIC		04/02		Modified from GP_MOPT		*
C************************************************************************
	CHARACTER*(*)	device, faxfil
C*
	LOGICAL		respnd
C*
C------------------------------------------------------------------------
C*
	iret = 0
C
	WRITE ( 6, 5000) device, faxfil
C*
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd ) THEN
	    CALL TM_ACCP ( ier )
	    IF ( ier .eq. 2 ) iret = -1
	END IF
C*
5000	FORMAT ( ' GPMAP PARAMETERS:',//
     +           ' Device:              ', A,/
     +           ' Faxfil:              ', A )
C*
	RETURN
	END
