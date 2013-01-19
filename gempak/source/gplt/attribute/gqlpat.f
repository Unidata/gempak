	SUBROUTINE GQLPAT  ( ilpat, iret )
C************************************************************************
C* GQLPAT								*
C*									*
C* This subroutine returns the software line pattern for the 		*
C* current line type.							*
C*									*
C* GQLPAT  ( ILPAT, IRET )						*
C*									*
C* Output parameters:							*
C*	ILPAT (8)	INTEGER		Line pattern values		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 8/90	Removed iltyp as output			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	INTEGER		ilpat (*)
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' ) THEN
	    DO  i = 1, 8
	        ilpat (i) = 0
	    END DO
	    iret = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    CALL DQLPAT  ( ilpat, iret )	    
	END IF
C*
	RETURN
	END
