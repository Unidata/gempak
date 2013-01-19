	SUBROUTINE GQHASH  ( szhsh, ihwid, ilwid, iret )
C************************************************************************
C* GQHASH 								*
C* 									*
C* This subroutine returns the current hash mark size, line width, and  *
C* line spacing.							*
C*									*
C* GQHASH  ( SZHSH, IHWID, ILWID, IRET )				*
C*									*
C* Output parameters:							*
C* 	SZHSH		REAL	 	Hash mark size multiplier	*
C*	IHWID		INTEGER		Hash mark line width 		*
C*	ILWID		INTEGER		Hash mark line spacing		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	03/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVSET.CMN'
C------------------------------------------------------------------------
C*	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    szhsh = 0.
	    iret   = NDVICE
C*
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    szhsh = shshsz
	    ihwid = lhwid
	    ilwid = llwidh
	    iret  = NORMAL
	END IF
C*
	RETURN
	END
