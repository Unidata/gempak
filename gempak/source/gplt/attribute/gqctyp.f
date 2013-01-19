	SUBROUTINE GQCTYP  ( szctyp, ictwid, iret)
C************************************************************************
C* GQCTYP								*
C*									*
C* This subroutine returns the current cloud type symbol size and	*
C* width multipliers.							*
C*									*
C* Note that this subroutine is not currently implemented.		*
C*									*
C* GQCTYP  ( SZCTYP, ICTWID, IRET)					*
C*									*
C* Output parameters:							*
C*	SZCTYP		REAL		Cloud type symbol size 		*
C*					multiplier			*
C*	ICTWID		INTEGER		Cloud type line width		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    szctyp = 0
	    ictwid = 0
	    iret   = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
            szctyp = sctsz
	    ictwid = lctwid
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
