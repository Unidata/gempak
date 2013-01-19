	SUBROUTINE DINITP ( iret )
C************************************************************************
C* DINITP								*
C*									*
C* This subroutine is called by each program that uses GEMPLT.		*
C*									*
C* DINITP ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT 3.1				*
C* M. desJardins/GSFC	 5/88	Documentation				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ADBUFF.CMN'
C
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = CINITP
C
 	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .eq. NORMAL )  THEN
	    CALL GGET  ( iret, 1, ierr )
	    IF  ( ierr .ne. NORMAL )  iret = ierr
	END IF
C*
	RETURN
	END
