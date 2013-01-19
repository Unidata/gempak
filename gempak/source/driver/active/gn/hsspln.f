	SUBROUTINE HSSPLN  ( msltyp, mslstr, msldir, tslsiz, mslwid,
     +			     iret )
C************************************************************************
C* HSSPLN - GN								*
C*									*
C* This subroutine sets the special line attributes.			*
C*									*
C* HSSPLN  ( MSLTYP, MSLSTR, MSLDIR, TSLSIZ, MSLWID, IRET )		*
C*									*
C* Input parameters:							*
C*	MSLTYP		INTEGER		Special line type		*
C*	MSLSTR		INTEGER		Special line stroke multiplier	*
C*	MSLDIR		INTEGER		Special line direction indicator*
C*	TSLSIZ		REAL		Special line size		*
C*	MSLWID		INTEGER		Special line width		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 4/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
