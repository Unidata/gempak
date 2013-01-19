	SUBROUTINE GSICMN  ( iret )
C************************************************************************
C* GSICMN								*
C*									*
C* This subroutine sets image common information in GEMPLT.		*
C*									*
C* GSICMN  ( IRET )							*
C*									*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 2/95						*
C************************************************************************
	INCLUDE 	'IMGDEF.CMN'
C*
C------------------------------------------------------------------------
C
	iret = 0
C
	CALL DSICMN ( ier )
C
	RETURN
	END
