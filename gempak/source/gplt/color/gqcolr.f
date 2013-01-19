	SUBROUTINE GQCOLR  ( icolr, iret )
C************************************************************************
C* GQCOLR								*
C* 									*
C* This subroutine returns the current color number.  			*
C* 									*
C* GQCOLR  ( ICOLR, IRET )						*
C*									*
C* Output parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVSET.CMN'
C-----------------------------------------------------------------------
C*	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    icolr = 0
	    iret  = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    icolr = lcolr
	    iret  = NORMAL
	END IF
C*
	RETURN
	END
