	SUBROUTINE DSICMN  ( iret )
C************************************************************************
C* DSICMN								*
C*									*
C* This subroutine sets image common information in the device driver	*
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
	INCLUDE		'ERROR.PRM'
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'IMGDEF.CMN'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2 + NIMCMN
	isend (2) = CSICMN
C
	CALL GPUT ( isend, 2, iret )
        IF  ( iret .ne. NORMAL )  RETURN
C
        CALL GPUT  ( imftyp, NIMCMN, iret )
        IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
