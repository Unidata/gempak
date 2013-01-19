	SUBROUTINE HSICMN ( iret )
C************************************************************************
C* HSICMN - XW								*
C*									*
C* This subroutine sets image common information.			*
C*									*
C* HSICMN (IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 3/95						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'IMGDEF.CMN'
C------------------------------------------------------------------------
C
C*	Pass the first variable of the common block
C
	CALL XSICMN ( imftyp, iret )
C
	RETURN
	END
