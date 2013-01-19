	SUBROUTINE GQCLR2  ( icolr, icolr2, iret )
C************************************************************************
C* GQCLR2								*
C* 									*
C* This subroutine returns the two current color numbers.  		*
C* 									*
C* GQCLR2  ( ICOLR, ICOLR2, IRET )					*
C*									*
C* Output parameters:							*
C* 	ICOLR		INTEGER		Color number 1			*
C* 	ICOLR2		INTEGER		Color number 2			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 3/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVSET.CMN'
C-----------------------------------------------------------------------
C*	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    icolr  = 0
	    icolr2 = 0
	    iret   = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    icolr  = lcolr
	    icolr2 = lcolr2
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
