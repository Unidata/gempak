	SUBROUTINE DSICMN  ( iret )
C************************************************************************
C* DSICMN								*
C*									*
C* This subroutine sets image common information in GEMPLT.		*
C*									*
C* DSICMN  ( IRET )							*
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
	CALL HSICMN ( iret )
C
	RETURN
	END
