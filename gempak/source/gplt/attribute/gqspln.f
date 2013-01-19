	SUBROUTINE GQSPLN  ( isltyp, islstr, isldir, slsiz, islwid,
     +			     iret )
C************************************************************************
C* GQSPLN								*
C*									*
C* This subroutine returns the current special line attributes 		*
C* including the special line type number, stroke multiplier, direction	*
C* indicator, size, and	width multiplier. 				*
C*									*
C* GQSPLN  ( ISLTYP, ISLSTR, ISLDIR, SLSIZ, ISLWID, IRET )		*
C*									*
C* Output parameters:							*
C*	ISLTYP		INTEGER		Special line type number	*
C*	ISLSTR		INTEGER		Special line stroke multiplier	*
C*	ISLDIR		INTEGER		Special line direction indicator*
C*	SLSIZ		REAL		Special line size		*
C*	ISLWID		INTEGER		Special line width multiplier	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 3/97		Copied from GQLINE		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	IF device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    isltyp  = 0
	    islstr  = 0
	    isldir  = 0
	    slsiz   = 0.
	    islwid  = 0
	    iret    = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    isltyp  = lsltyp
	    islstr  = lslstr
	    isldir  = lsldir
	    slsiz   = sslsiz
	    islwid  = lslwid
	    iret    = NORMAL
	END IF
C*
	RETURN
	END
