	SUBROUTINE GQNCOL  ( ncolr, iret )
C************************************************************************
C* GQNCOL								*
C* 									*
C* This subroutine returns the number of colors on the current device.	*
C* 									*
C* GQNCOL  ( NCOLR, IRET )						*
C*									*
C* Output parameters:							*
C* 									*
C* 	NCOLR		INTEGER		Color number			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	 1/92	New subroutine				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C-----------------------------------------------------------------------
C*	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    ncolr = 0
	    iret  = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVCHR/.
C
	    ncolr = nncolr
	    iret  = NORMAL
	END IF
C*
	RETURN
	END
