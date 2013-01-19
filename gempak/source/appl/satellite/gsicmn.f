	SUBROUTINE GSICMN  ( iret )
C************************************************************************
C* GSICMN								*
C*									*
C* This subroutine sends image common information to GEMPLT.		*
C*									*
C* GSICMN  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 2/95						*
C* T. Piper/GSC		 5/98		Corrected prolog                *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	INTEGER   	isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2 + NIMCMN
	isend (2) = FSICMN
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( imftyp, NIMCMN, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	RETURN
	END
